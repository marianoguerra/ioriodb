-module(iorio_vnode_buckets).
-behaviour(gen_server).

-export([start_link/1, put/5, put/6, raw_put/5, get/6, bucket_size/2,
         check_evict/2, truncate_percentage/3, clean/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {gblobs, gblob_config_fun, path,
                partition,
                next_bucket_index=1,
                max_bucket_time_no_evict_ms=60000,
                max_bucket_size_bytes=52428800}).

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

handle_call({truncate_percentage, BucketName, Percentage}, _From, State) ->
    {NewState, Reply} = do_truncate_percentage(State, BucketName, Percentage),
    {reply, Reply, NewState};

handle_call(clean, _From, State=#state{gblobs=Gblobs}) ->
    {ok, NewGblobs} = rscbag:clean(Gblobs),
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
    lager:debug("put ~p", [{ReqId, BucketName, Stream}]),
    Timestamp = sblob_util:now(),
    {NewState, GBlob} = get_gblob(State, BucketName, Stream),

    Result = if
                 LastSeqNum == nil ->
                     gblob_server:put(GBlob, Timestamp, Data);
                 true ->
                     gblob_server:put(GBlob, Timestamp, Data, LastSeqNum)
             end,

    Reply = {ReqId, Result},
    {Reply, NewState}.


do_raw_put(State, BucketName, Stream, Timestamp, Data) ->
    {NewState, GBlob} = get_gblob(State, BucketName, Stream),
    Entry = gblob_server:put(GBlob, Timestamp, Data),
    {Entry, NewState}.

do_get(State=#state{partition=Partition}, BucketName, Stream, From, Count, Callback) ->
    lager:debug("get ~s ~s ~p ~p at ~p",
                [BucketName, Stream, From, Count, Partition]),
    {NewState, GBlob} = get_gblob(State, BucketName, Stream),

    spawn(fun () ->
                  Entries = gblob_server:get(GBlob, From, Count),
                  Callback(Entries)
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
