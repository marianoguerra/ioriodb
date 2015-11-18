-module(iorio_rlimit).

-behaviour(gen_server).

-export([start_link/1, notify/2, set_limits/1, set_limit/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {bcast_interval_ms, op_weights, table, node, limits,
                key_interval_secs=60, last_bcast=0}).

%% Public API

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

notify(Resource, Action) ->
    gen_server:call(?MODULE, {notify, Resource, Action}).

set_limits(Limits) ->
    gen_server:call(?MODULE, {set_limits, Limits}).

set_limit(Resource, Limit) ->
    gen_server:call(?MODULE, {set_limit, Resource, Limit}).

%% Server implementation, a.k.a.: callbacks

init(Opts) ->
    BcastIntervalMs = proplists:get_value(bcast_interval_ms, Opts, 60000),
    KeyInterval = proplists:get_value(key_interval_secs, Opts, 60),
    OpWeights = maps:from_list(proplists:get_value(op_weights, Opts, [])),
    Table = ets:new(iorio_rlimit, [set, {write_concurrency, false},
                                   {read_concurrency, false}]),
    Node = erlang:node(),
    State = #state{bcast_interval_ms=BcastIntervalMs, op_weights=OpWeights,
                   key_interval_secs=KeyInterval, table=Table, node=Node,
                   limits=#{}},
    {ok, State}.

handle_call({notify, Resource, Action}, _From,
            State=#state{op_weights=OpWeights, table=Table, node=Node,
                         key_interval_secs=KeyIntervalSecs, limits=Limits}) ->
    Weight = maps:get(Action, OpWeights, 1),
    Now = now_secs(),
    Key = {Node, actions, Resource},
    NewVal = case ets:lookup(Table, Key) of
                 [] ->
                     {Weight, Now};
                 [{_, {CurWeight, LastUpdateS}}] ->
                     if Now - LastUpdateS < KeyIntervalSecs ->
                            {CurWeight + Weight, LastUpdateS};
                        true ->
                            {Weight, Now}
                     end
             end,

    {NewWeight, _} = NewVal,
    Entry = {Key, NewVal},
    ets:insert(Table, Entry),

    Limit = maps:get(Resource, Limits, undefined),

    Status = if Limit == undefined -> ok;
                Limit < NewWeight -> over_limit;
                true -> ok
             end,


    {reply, {Status, NewVal}, State};

handle_call({set_limits, Limits}, _From, State) ->
    {reply, ok, State#state{limits=Limits}};

handle_call({set_limit, Resource, Limit}, _From, State=#state{limits=Limits}) ->
    NewLimits = maps:put(Resource, Limit, Limits),
    {reply, ok, State#state{limits=NewLimits}};

handle_call(Msg, _From, State) ->
    lager:warning("iorio_rlimit: Unexpected handle call message: ~p",[Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:warning("iorio_rlimit: Unexpected handle cast message: ~p",[Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    lager:warning("iorio_rlimit: Unexpected handle info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private api
now_secs() ->
    {MegaSecs, Secs, _MicroSecs} = os:timestamp(),
    (MegaSecs * 1000000 + Secs).
