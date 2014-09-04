-module(iorio_write_fsm).
-behavior(gen_fsm).
-include("iorio.hrl").

%% API
-export([start_link/7, write/5]).

%% Callbacks
-export([init/1, code_change/4, handle_event/3, handle_info/3,
         handle_sync_event/4, terminate/3]).

%% States
-export([prepare/2, execute/2, waiting/2]).

%% req_id: The request id so the caller can verify the response.
%%
%% sender: The pid of the sender so a reply can be made.
%%
%% prelist: The preflist for the given {Bucket, Stream} pair.
%%
%% num_w: The number of successful write replies.
-record(state, {req_id :: pos_integer(),
                from :: pid(),
                n :: pos_integer(),
                w :: pos_integer(),
                bucket :: string(),
                stream :: string(),
                data = undefined :: term() | undefined,
                preflist :: riak_core_apl:preflist2(),
                num_w = 0 :: non_neg_integer()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ReqID, From, Bucket, Stream, Data, N, W) ->
    gen_fsm:start_link(?MODULE, [ReqID, From, Bucket, Stream, Data, N, W], []).

write(N, W, Bucket, Stream, Data) ->
    ReqID = iorio_util:reqid(),
    iorio_write_fsm_sup:start_write_fsm([ReqID, self(), Bucket, Stream, Data, N, W]),
    {ok, ReqID}.

%%%===================================================================
%%% States
%%%===================================================================

%% @doc Initialize the state data.
init([ReqID, From, Bucket, Stream, Data, N, W]) ->
    SD = #state{req_id=ReqID, from=From, n=N, w=W,
                bucket=Bucket, stream=Stream, data=Data},
    {ok, prepare, SD, 0}.

%% @doc Prepare the write by calculating the _preference list_.
prepare(timeout, SD0=#state{bucket=Bucket, n=N, stream=Stream}) ->
    DocIdx = riak_core_util:chash_key({Bucket, Stream}),
    Preflist = riak_core_apl:get_apl(DocIdx, N, iorio),
    SD = SD0#state{preflist=Preflist},
    {next_state, execute, SD, 0}.

%% @doc Execute the write request and then go into waiting state to
%% verify it has meets consistency requirements.
execute(timeout, SD0=#state{req_id=ReqID,
                            bucket=Bucket,
                            stream=Stream,
                            data=Data,
                            preflist=Preflist}) ->
    Command = {put, ReqID, Bucket, Stream, Data},
    riak_core_vnode_master:command(Preflist, Command, {fsm, undefined, self()},
                                   iorio_vnode_master),
    {next_state, waiting, SD0}.

%% @doc Wait for W write reqs to respond.
waiting({ReqID, Entry}, SD0=#state{from=From, num_w=NumW0, w=W}) ->
    NumW = NumW0 + 1,
    SD = SD0#state{num_w=NumW},
    if
        NumW =:= W ->
            From ! {ReqID, Entry},
            {stop, normal, SD};
        true -> {next_state, waiting, SD}
    end.

handle_info(Info, _StateName, StateData) ->
    lager:warning("got unexpected info ~p", [Info]),
    {stop,badmsg,StateData}.

handle_event(Event, _StateName, StateData) ->
    lager:warning("got unexpected event ~p", [Event]),
    {stop,badmsg,StateData}.

handle_sync_event(Event, _From, _StateName, StateData) ->
    lager:warning("got unexpected sync event ~p", [Event]),
    {stop,badmsg,StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
    ok.
