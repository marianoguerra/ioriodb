-module(iorio_vnode_buckets).
-behaviour(gen_server).

-export([start_link/1, put/5, put/6, raw_put/5, get/6, bucket_size/2,
         delete/3, check_evict/2, truncate_percentage/3, clean/1,
         send_metrics/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {gblobs, gblob_config_fun, path,
                partition,
                next_bucket_index=1,
                max_bucket_time_no_evict_ms=60000,
                max_bucket_size_bytes=52428800}).
-record(metrics_state, {count=0, active=0, inactive=0, now,
                        no_action=0, no_eviction=0, no_check=0}).

%% Public API

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

clean(Pid) ->
    gen_server:call(Pid, clean).

put(Ref, ReqId, BucketName, Stream, Data) ->
    put(Ref, ReqId, BucketName, Stream, Data, nil).

put(Ref, ReqId, BucketName, Stream, Data, LastSeqNum) ->
    gen_server:call(Ref, {put, ReqId, BucketName, Stream, Data, LastSeqNum}).

raw_put(Ref, BucketName, Stream, Timestamp, Data) ->
    gen_server:call(Ref, {raw_put, BucketName, Stream, Timestamp, Data}).

check_evict(Ref, Fun) ->
    gen_server:call(Ref, {check_evict, Fun}).

bucket_size(Ref, BucketName) ->
    gen_server:call(Ref, {bucket_size, BucketName}).

delete(Ref, BucketName, Stream) ->
    gen_server:call(Ref, {delete, BucketName, Stream}).

send_metrics(Ref) ->
    gen_server:call(Ref, send_metrics).

truncate_percentage(Ref, BucketName, Percentage) ->
    gen_server:call(Ref, {truncate_percentage, BucketName, Percentage}).


get(Ref, BucketName, Stream, From, Count, Callback) ->
    gen_server:cast(Ref, {get, BucketName, Stream, From, Count, Callback}).

%% Server implementation, a.k.a.: callbacks

init(Opts) ->
    MaxBucketSizeBytes = application:get_env(iorio, max_bucket_size_bytes, 52428800),

    {path, Path} = proplists:lookup(path, Opts),
    {partition, Partition} = proplists:lookup(partition, Opts),

    GBlobsOpts = [{resource_handler, iorio_rhandler_gblob_server}, {kv_mod, rscbag_ets}],
    {ok, Gblobs} = rscbag:init(GBlobsOpts),

    GblobConfigFun = proplists:get_value(gblob_config_fun, Opts,
                                         fun default_gblob_config/4),

    State = #state{max_bucket_size_bytes=MaxBucketSizeBytes, path=Path,
                   partition=Partition,
                   gblob_config_fun=GblobConfigFun, gblobs=Gblobs},
    {ok, State}.

handle_call({put, ReqId, BucketName, Stream, Data, LastSeqNum}, _From, State) ->
    {Reply, NewState} = do_put(State, ReqId, BucketName, Stream, Data, LastSeqNum),
    {reply, Reply, NewState};

handle_call({raw_put, BucketName, Stream, Timestamp, Data}, _From, State) ->
    {Reply, NewState} = do_raw_put(State, BucketName, Stream, Timestamp, Data),
    {reply, Reply, NewState};

handle_call({check_evict, Fun}, _From, State) ->
    {Reply, NewState} = do_check_evict(State, Fun),
    {reply, Reply, NewState};

handle_call({bucket_size, BucketName}, _From, State) ->
    {NewState, Reply} = do_bucket_size(State, BucketName),
    {reply, Reply, NewState};

handle_call({delete, BucketName, Stream}, _From, State) ->
    {NewState, Reply} = do_stream_delete(State, BucketName, Stream),
    {reply, Reply, NewState};

handle_call({truncate_percentage, BucketName, Percentage}, _From, State) ->
    {NewState, Reply} = do_truncate_percentage(State, BucketName, Percentage),
    {reply, Reply, NewState};

handle_call(clean, _From, State=#state{gblobs=Gblobs}) ->
    {ok, NewGblobs} = rscbag:clean(Gblobs),
    {reply, ok, State#state{gblobs=NewGblobs}};

handle_call(send_metrics, _From, State=#state{gblobs=Gblobs}) ->
    {ok, NewGblobs, Result} = rscbag:foldl(Gblobs, fun calculate_metrics/3,
                                          metrics_foldl_initial_state()),
    #metrics_state{count=Count, active=ActiveCount, inactive=InactiveCount,
                   no_action=NoAction, no_check=NoCheck, no_eviction=NoEviction} = Result,
    iorio_stats:bucket_per_vnode(Count),
    iorio_stats:bucket_active(ActiveCount),
    iorio_stats:bucket_inactive(InactiveCount),
    iorio_stats:bucket_no_action(NoAction),
    iorio_stats:bucket_no_check(NoCheck),
    iorio_stats:bucket_no_eviction(NoEviction),
    {reply, ok, State#state{gblobs=NewGblobs}};

handle_call(Msg, _From, State) ->
    lager:warning("vnode_buckets: Unexpected handle call message: ~p",[Msg]),
    {reply, ok, State}.


handle_cast({get, BucketName, Stream, From, Count, Callback}, State) ->
    NewState = do_get(State, BucketName, Stream, From, Count, Callback),
    {noreply, NewState};

handle_cast(Msg, State) ->
    lager:warning("vnode_buckets: Unexpected handle cast message: ~p",[Msg]),
    {noreply, State}.


handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State=#state{gblobs=GBlobs}) ->
    NewGBlobs = remove_bucket(GBlobs, Pid),
    {noreply, State#state{gblobs=NewGBlobs}};

handle_info(Msg, State) ->
    lager:warning("vnode_buckets: Unexpected handle info message: ~p", [Msg]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private api

remove_bucket(Buckets, Pid) ->
    case rscbag:remove_by_val(Buckets, Pid, false) of
        {ok, NewBuckets} ->
            NewBuckets;
        {{error, Reason}, NewBuckets} ->
            lager:warning("error removing bucket: ~p ~p", [Pid, Reason]),
            NewBuckets
    end.

stream_size(Path, Bucket, Stream) ->
    FullPath = filename:join([Path, Bucket, Stream]),
    sblob_util:deep_size(FullPath).

do_bucket_size(State=#state{path=Path}, Bucket) ->
    Streams = iorio_bucket:list_streams(Path, Bucket),
    R = lists:foldl(fun (Stream, {TotalSize, Sizes}) ->
                            StreamSize = stream_size(Path, Bucket, Stream),
                            NewTotalSize = TotalSize + StreamSize,
                            NewSizes = [{Stream, StreamSize}|Sizes],
                            {NewTotalSize, NewSizes}
                    end, {0, []}, Streams),
    {State, R}.

do_stream_delete(State=#state{}, BucketName, Stream) ->
    {NewState, GBlob} = get_gblob(State, BucketName, Stream),
    gblob_server:delete(GBlob),
    {NewState, ok}.

do_truncate_percentage(State=#state{path=Path}, BucketName, Percentage) ->
    HaveBucket = iorio_bucket:have_bucket(Path, BucketName),
    if HaveBucket ->
           TruncateGblobs = fun ({_Id, Gblob}) ->
                                    gblob_server:truncate_percentage(Gblob, Percentage)
                            end,
           {NewState, Result} = map_gblobs(State, BucketName, TruncateGblobs),
           {NewState, lists:reverse(Result)};
       true -> {State, notfound}
    end.

map_gblobs(State=#state{path=Path}, BucketName, Fun) ->
    Streams = iorio_bucket:list_streams(Path, BucketName),
    lists:foldl(fun (Stream, {StateIn, ResultIn}) ->
                        {StateOut , GBlob} = get_gblob(StateIn, BucketName, Stream),
                        Result = Fun({{BucketName, Stream}, GBlob}),
                        ResultOut = [Result|ResultIn],
                        {StateOut, ResultOut}
                end, {State, []}, Streams).


do_put(State=#state{}, ReqId, BucketName, Stream, Data, LastSeqNum) ->
    Timestamp = sblob_util:now(),
    {NewState, GBlob} = get_gblob(State, BucketName, Stream),

    Result = try
                 if
                     LastSeqNum == nil ->
                         gblob_server:put(GBlob, Timestamp, Data);
                     true ->
                         gblob_server:put(GBlob, Timestamp, Data, LastSeqNum)
                 end
             catch
                 Type:Error -> {error, {Type, Error}}
             end,

    Reply = {ReqId, Result},
    {Reply, NewState}.


do_raw_put(State, BucketName, Stream, Timestamp, Data) ->
    {NewState, GBlob} = get_gblob(State, BucketName, Stream),
    Reply = try gblob_server:put(GBlob, Timestamp, Data)
            catch Type:Error -> {error, {Type, Error}} end,
    {Reply, NewState}.

do_get(State=#state{}, BucketName, Stream, From, Count, Callback) ->
    {NewState, GBlob} = get_gblob(State, BucketName, Stream),

    spawn(fun () ->
                  try
                      Entries = gblob_server:get(GBlob, From, Count),
                      Callback(Entries)
                  catch
                      Type:Error -> Callback({error, {Type, Error}})
                  end
          end),

    NewState.

do_check_evict(State=#state{partition=Partition, path=Path,
                            next_bucket_index=NextBucketIndex,
                            max_bucket_time_no_evict_ms=MaxTimeMsNoEviction,
                            max_bucket_size_bytes=MaxBucketSize},
               EvictFun) ->

    BucketNames = lists:sort(iorio_bucket:list_buckets(Path)),
    BucketCount = length(BucketNames),
    NewNextIndex = if BucketCount == 0 ->
                          NextBucketIndex;
                      BucketCount > NextBucketIndex ->
                          1;
                      true ->
                          NextBucketIndex + 1
                   end,

    Result = if NextBucketIndex > BucketCount ->
                    lager:debug("no eviction, no buckets in vnode ~p",
                                [Partition]),
                    ok;
                true ->
                    BucketName = lists:nth(NextBucketIndex, BucketNames),
                    EvictFun(BucketName, Partition, MaxBucketSize,
                             MaxTimeMsNoEviction)
             end,
    NewState = State#state{next_bucket_index=NewNextIndex},
    {Result, NewState}.

make_get_gblob_opts(PartitionStr, BucketName, Stream, GblobConfigFun) ->
    fun () ->
            Path = filename:join([PartitionStr, BucketName, Stream]),
            GblobConfigFun(PartitionStr, BucketName, Stream, Path)
    end.

default_gblob_config(_PartitionStr, Bucket, Stream, Path) ->
    iorio_config:stream_config(Path, Bucket, Stream).

get_gblob(State=#state{gblobs=GBlobs, path=Path,
                       gblob_config_fun=GblobConfigFun}, BucketName, Stream) ->
    Key = {BucketName, Stream},
    GetBucketOpts = make_get_gblob_opts(Path, BucketName, Stream, GblobConfigFun),
    case rscbag:get(GBlobs, Key, GetBucketOpts) of
        {{ok, created, Gb}, Gs} ->
            erlang:monitor(process, Gb),
            {State#state{gblobs=Gs}, Gb};
        {{ok, found, Gb}, Gs} ->
            {State#state{gblobs=Gs}, Gb};
        Error -> {error, Error, State}
    end.

metrics_foldl_initial_state() -> #metrics_state{now=sblob_util:now_fast()}.

calculate_metrics({_Bucket, _Key}, Bucket,
                  State0=#metrics_state{count=Count, active=ActiveCount,
                                        inactive=InactiveCount, now=Now,
                                        no_action=NoActionCount,
                                        no_eviction=NoEvictionCount,
                                        no_check=NoCheckCount}) ->
    {Active, LastAction, LastEviction, LastCheck} = gblob_server:status(Bucket),

    {NewActiveCount, NewInactiveCount} = if Active -> {ActiveCount + 1, InactiveCount};
                                            true -> {ActiveCount, InactiveCount + 1}
                                         end,
    NewCount = Count + 1,

    NewNoActionCount = if LastAction /= 0 ->
                              iorio_stats:bucket_last_action(Now - LastAction),
                              NoActionCount;
                          true -> NoActionCount + 1
                       end,
    NewNoEvictionCount = if LastEviction /= 0 ->
                                iorio_stats:bucket_last_eviction(Now - LastEviction),
                                NoEvictionCount;
                            true -> NoEvictionCount + 1
                         end,
    NewNoCheckCount = if LastCheck /= 0 ->
                             iorio_stats:bucket_last_check(Now - LastCheck),
                             NoCheckCount;
                         true -> NoCheckCount + 1
                      end,

    State0#metrics_state{count=NewCount, active=NewActiveCount,
                         inactive=NewInactiveCount,
                         no_action=NewNoActionCount,
                         no_eviction=NewNoEvictionCount,
                         no_check=NewNoCheckCount}.
