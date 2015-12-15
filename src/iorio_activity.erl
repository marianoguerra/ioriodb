-module(iorio_activity).

-behaviour(gen_server).

-export([start_link/1, notify/2, get_stats_and_clean/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {table, node}).

%% Public API

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

notify(Type, Data) ->
    gen_server:call(?MODULE, {notify, Type, Data}).

get_stats_and_clean(Limit) ->
    gen_server:call(?MODULE, {get_stats_and_clean, Limit}).

%% Server implementation, a.k.a.: callbacks

init(_Opts) ->
    Table = ets:new(iorio_activity, [set, {write_concurrency, false},
                                     {read_concurrency, false}]),
    Node = erlang:node(),
    State = #state{table=Table, node=Node},
    {ok, State}.

handle_call({notify, Type, Key}, _From, State=#state{table=Table}) ->
    K = {Type, Key},
    Result = ets:update_counter(Table, K, 1, {K, 0}),
    {reply, {ok, Result}, State};

handle_call({get_stats_and_clean, _Limit}, _From, State=#state{table=Table}) ->
    Result = ets:foldl(fun fold_items/2, #{}, Table),
    ets:delete_all_objects(Table),
    {reply, {ok, Result}, State};

handle_call(Msg, _From, State) ->
    lager:warning("iorio_activity: Unexpected handle call message: ~p",[Msg]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    lager:warning("iorio_activity: Unexpected handle cast message: ~p",[Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    lager:warning("iorio_activity: Unexpected handle info message: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private api
fold_items({{Type, Key}, Count}, AccIn) ->
    TypeItems = maps:get(Type, AccIn, []),
    maps:put(Type, [#{key => tuple_to_list(Key), count => Count}|TypeItems], AccIn).
    

