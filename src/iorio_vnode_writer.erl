-module(iorio_vnode_writer).
-behaviour(gen_server).

-export([start_link/0, reply_to/6]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {receive_timeout_ms=5000}).
%% Public API

start_link() ->
    gen_server:start_link(?MODULE, [], []).

reply_to(Writer, ReplyTo, ReqId, M, F, A) ->
    gen_server:cast(Writer, {reply_to, ReplyTo, ReqId, {M, F, A}}).

%% Server implementation, a.k.a.: callbacks

init([]) ->
    State = #state{},
    {ok, State}.

handle_call(Msg, _From, State) ->
    lager:warning("Unexpected handle call message: ~p~n",[Msg]),
    {reply, ok, State}.

handle_cast({reply_to, Pid, ReqId, {M, F, A}},
            State=#state{receive_timeout_ms=ReceiveTimeoutMs}) ->
    try
        apply(M, F, A),
        receive
            {ReqId, _Val}=Result ->
                Pid ! Result
        after ReceiveTimeoutMs ->
                  Pid ! {error, timeout}
        end
    catch
        T:E ->
            lager:warning("error running task ~p ~p", [T, E]),
            Pid ! {error, {T, E}}
    end,

    {noreply, State};

handle_cast(Msg, State) ->
    lager:warning("Unexpected handle cast message: ~p~n",[Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    lageer:warning("Unexpected handle info message: ~p~n",[Msg]),
    {noreply, State}.


terminate(_Reason, _Gblob) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private api
