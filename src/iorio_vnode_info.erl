-module(iorio_vnode_info).
-behaviour(gen_server).

-export([start_link/1, list_buckets/1, list_streams/2, bucket_info/2, check/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {path, partition, buckets, timeout, timer_ref, last_update,
                size=0}).

%% Public API

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

list_buckets(Ref) ->
    gen_server:call(Ref, {list_buckets}).

list_streams(Ref, BucketName) ->
    gen_server:call(Ref, {list_streams, ensure_binary(BucketName)}).

bucket_info(Ref, BucketName) ->
    gen_server:call(Ref, {bucket_info, ensure_binary(BucketName)}).

check(Ref) ->
    gen_server:call(Ref, {check}).


%% Server implementation, a.k.a.: callbacks

init(Opts) ->
    {path, Path} = proplists:lookup(path, Opts),
    {partition, Partition} = proplists:lookup(partition, Opts),
    Timeout = proplists:get_value(timeout, Opts, 30000),

    State = #state{path=Path, partition=Partition, timeout=Timeout,
                   buckets=#{}},

    {ok, set_timer(State)}.

handle_call(Command, _From, State=#state{last_update=undefined})
        when Command =/= {check} ->
    NewState = do_check(State),
    handle_call(Command, _From, NewState);

handle_call({list_buckets}, _From, State=#state{buckets=Buckets}) ->
    {reply, {ok, Buckets}, State};

handle_call({check}, _From, State=#state{}) ->
    NewState = do_check(State),
    {reply, ok, NewState};

handle_call({list_streams, BucketName}, _From, State=#state{buckets=Buckets}) ->
    #{streams := Streams} = maps:get(BucketName, Buckets, #{streams => #{}}),
    {reply, {ok, Streams}, State};

handle_call({bucket_info, BucketName}, _From, State=#state{buckets=Buckets}) ->
    R = case maps:find(BucketName, Buckets) of
        {ok, _Bucket}=R1 -> R1;
        error -> {error, not_found}
    end,
    {reply, R, State};

handle_call(Msg, _From, State) ->
    lager:warning("vnode_info: Unexpected handle call message: ~p",[Msg]),
    {reply, ok, State}.


handle_cast(Msg, State) ->
    lager:warning("vnode_info: Unexpected handle cast message: ~p",[Msg]),
    {noreply, State}.


handle_info({timeout, TimerRef, timer}, State=#state{timer_ref=TimerRef}) ->
    NewState = do_check(State),
    {noreply, set_timer(NewState)};

handle_info(Msg, State) ->
    lager:warning("vnode_buckets: Unexpected handle info message: ~p", [Msg]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private api

set_timer(State=#state{timeout=Timeout}) ->
    TimerRef = erlang:start_timer(Timeout, self(), timer),
    State#state{timer_ref=TimerRef}.

list_dir(Path) -> file:list_dir(Path).

get_stream_info(BasePath, StreamName) ->
    Path = filename:join(BasePath, StreamName),
    Size = sblob_util:deep_size(Path),
    #{name => list_to_binary(StreamName), size => Size}.

fold_streams(StreamName, {Path, CurSize, StreamsIn}) ->
    StreamInfo = get_stream_info(Path, StreamName),
    StreamOut = maps:put(list_to_binary(StreamName), StreamInfo, StreamsIn),
    #{size := StreamSize} = StreamInfo,
    SizeOut = CurSize + StreamSize,
    {Path, SizeOut, StreamOut}.

get_bucket_info(BasePath, BucketName) ->
    Path = filename:join(BasePath, BucketName),
    {ok, StreamNames} = list_dir(Path),
    Fun = fun fold_streams/2,
    {_Path, Size, NewStreams} = lists:foldl(Fun, {Path, 0, #{}}, StreamNames),
    #{streams => NewStreams, size => Size}.

fold_buckets(BucketName, {Path, CurSize, BucketsIn}) ->
    BucketInfo = get_bucket_info(Path, BucketName),
    #{size := BucketSize} = BucketInfo,
    BucketsOut = maps:put(list_to_binary(BucketName), BucketInfo, BucketsIn),
    SizeOut = CurSize + BucketSize,
    {Path, SizeOut, BucketsOut}.

check_path(Path) ->
    case list_dir(Path) of
      {ok, BucketNames} -> 
        Fun = fun fold_buckets/2,
        {_Path, Size, Buckets} = lists:foldl(Fun, {Path, 0, #{}}, BucketNames),
        {Size, Buckets};
      {error, Reason} ->
	lager:error("check path ~p: ~p", [Path, Reason]),
	{0, #{}}
    end.

do_check(State=#state{path=Path}) ->
    Now = os:timestamp(),
    {Size, NewBuckets} = check_path(Path),
    State#state{buckets=NewBuckets, last_update=Now, size=Size}.

ensure_binary(Val) when is_binary(Val) -> Val;
ensure_binary(Val) when is_list(Val) -> list_to_binary(Val).
