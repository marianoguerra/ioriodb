-module(iorio_node).

-export([init/1, ping/1, put/5, coord_put_conditionally/8, coord_put/7,
         delete/4,
         put_conditionally/6, get/6, subscribe/5, unsubscribe/4,
         evict_bucket/1, send_metrics/1, free_resources/1, truncate_percentage/4,
         bucket_size/3, list_buckets/2, list_streams/3, delete/1]).

-include_lib("sblob/include/sblob.hrl").
-include("include/iorio_node_state.hrl").

init(Opts) ->
    Partition = proplists:get_value(partition, Opts),
    BasePath = iorio_bucket:base_path(),
    {ok, WriterPid} = iorio_vnode_writer:start_link(),

    PartitionStr = integer_to_list(Partition),
    Path = filename:join([BasePath, PartitionStr]),

    % TODO: calculate based on number of buckets
    BucketEvictTimeInterval = application:get_env(iorio, bucket_evict_time_ms, 60000),
    VnodeInfoTimeout = application:get_env(iorio, vnode_info_timeout_ms, 30000),

    BucketsOpts = [{path, Path}, {partition, Partition}],
    VnodeInfoOpts = [{path, Path}, {partition, Partition},
                     {timeout, VnodeInfoTimeout}],

    {ok, Channels}  = iorio_vnode_channels:start_link(),
    {ok, Buckets}   = iorio_vnode_buckets:start_link(BucketsOpts),
    {ok, VnodeInfo} = iorio_vnode_info:start_link(VnodeInfoOpts),

    Pid = self(),
    spawn(fun () ->
                  % distribute intervals to avoid calling all of them at once
                  % on the same node
                  Now = os:timestamp(),
                  {RandomSleep, _} = random:uniform_s(BucketEvictTimeInterval, Now),
                  timer:sleep(RandomSleep),
                  {ok, _TimerRef} = timer:send_interval(BucketEvictTimeInterval,
                                                        Pid, evict_bucket)
          end),

    MetricsInterval = 60000,
    {ok, _TimerRef} = timer:send_interval(MetricsInterval, Pid, send_metrics),

    State = #state{partition=Partition, path=Path, writer=WriterPid,
                   buckets=Buckets, channels=Channels, vnode_info=VnodeInfo},
    {ok, State}.

ping(State=#state{partition=Partition}) ->
    {reply, {pong, Partition}, State}.

put(State=#state{buckets=Buckets, channels=Channels}, ReqId, BucketName, Stream, Data) ->
    Reply = iorio_vnode_buckets:put(Buckets, ReqId, BucketName, Stream, Data),
    send_to_channel(Reply, Channels, BucketName, Stream),
    {reply, Reply, State}.

coord_put_conditionally(State=#state{writer=Writer}, N, W, Bucket, Stream,
                        Data, LastSeqNum, Pid) ->
    ReqId = iorio_util:reqid(),
    iorio_vnode_writer:reply_to(Writer, Pid, ReqId,
                                iorio_write_fsm, write_conditionally,
                                [N, W, Bucket, Stream, Data, LastSeqNum,
                                 Writer, ReqId]),
    {reply, ReqId, State}.


coord_put(State=#state{writer=Writer}, N, W, Bucket, Stream, Data, Pid) ->
    ReqId = iorio_util:reqid(),
    iorio_vnode_writer:reply_to(Writer, Pid, ReqId, iorio_write_fsm, write,
                                [N, W, Bucket, Stream, Data, Writer, ReqId]),
    {reply, ReqId, State}.

put_conditionally(State=#state{buckets=Buckets, channels=Channels}, ReqId,
                  BucketName, Stream, Data, LastSeqNum) ->
    Reply = iorio_vnode_buckets:put(Buckets, ReqId, BucketName, Stream, Data, LastSeqNum),
    send_to_channel(Reply, Channels, BucketName, Stream),
    {reply, Reply, State}.

get(State=#state{buckets=Buckets}, BucketName, Stream, From, Count, Callback) ->
    iorio_vnode_buckets:get(Buckets, BucketName, Stream, From, Count, Callback),
    {noreply, State}.

delete(State=#state{buckets=Buckets}, ReqId, BucketName, Stream) ->
    lager:info("iorio_node:delete ~p/~p", [BucketName, Stream]),
    iorio_vnode_buckets:delete(Buckets, BucketName, Stream),
    {reply, {ReqId, nil}, State}.

subscribe(State=#state{channels=Channels}, BucketName, Stream, FromSeqNum, Pid) ->
    iorio_vnode_channels:subscribe(Channels, BucketName, Stream, FromSeqNum, Pid),
    {reply, ok, State}.

unsubscribe(State=#state{channels=Channels}, BucketName, Stream, Pid) ->
    iorio_vnode_channels:unsubscribe(Channels, BucketName, Stream, Pid),
    {reply, ok, State}.

evict_bucket(State=#state{buckets=Buckets}) ->
    EvictFun = fun evict_bucket/4,
    iorio_vnode_buckets:check_evict(Buckets, EvictFun),
    {ok, State}.

send_metrics(State=#state{channels=Channels}) ->
    iorio_vnode_channels:send_metrics(Channels),
    {ok, State}.

truncate_percentage(State=#state{buckets=Buckets}, BucketName, Percentage, RefId) ->
    Result = iorio_vnode_buckets:truncate_percentage(Buckets, BucketName, Percentage),
    {reply, {RefId, Result}, State}.

bucket_size(State=#state{vnode_info=VnodeInfo}, BucketName, RefId) ->
    case iorio_vnode_info:bucket_info(VnodeInfo, BucketName) of
    {ok, #{size := Size, streams := Streams}} ->
            Fun = fun (StreamName, #{size := StreamSize}, AccIn) ->
                          [{StreamName, StreamSize}|AccIn]
                  end,
            StreamNamesAndSizes = maps:fold(Fun, [], Streams),
            Result = {Size, StreamNamesAndSizes},
            {reply, {RefId, Result}, State};
    {error, not_found} ->
            {reply, {RefId, {0, []}}, State}
    end.

list_buckets(State=#state{vnode_info=VnodeInfo}, RefId) ->
    {ok, Buckets} = iorio_vnode_info:list_buckets(VnodeInfo),
    {reply, {RefId, Buckets}, State}.

list_streams(State=#state{vnode_info=VnodeInfo}, Bucket, RefId) ->
    {ok, Streams} = iorio_vnode_info:list_streams(VnodeInfo, Bucket),
    {reply, {RefId, Streams}, State}.

delete(State=#state{path=Path}) ->
    iorio_bucket:delete(Path),
    {ok, State}.

free_resources(#state{buckets=Buckets, channels=Chans}) ->
    iorio_vnode_channels:clean(Chans),
    iorio_vnode_buckets:clean(Buckets).

% private functions

send_to_channel({_ReqId, {error, _Reason}}, _Channels, _BucketName, _Stream) ->
    ok;
send_to_channel({_ReqId, Entry=#sblob_entry{}}, Channels, BucketName, Stream) ->
    iorio_vnode_channels:send(Channels, BucketName, Stream, Entry).

should_evict(BucketName, MaxTimeMsNoEviction) ->
    Now = sblob_util:now_fast(),
    LastEviction = get_last_eviction(BucketName),
    ShouldEvict = (LastEviction + MaxTimeMsNoEviction) < Now,
    ShouldEvict.

get_last_eviction(BucketName) ->
    riak_core_metadata:get({<<"bucket">>, <<"eviction">>}, BucketName,
                           [{default, 0}]).

set_last_eviction(BucketName) ->
    Now = sblob_util:now_fast(),
    riak_core_metadata:put({<<"bucket">>, <<"eviction">>}, BucketName, Now),
    Now.

evict_bucket(BucketName, Partition, MaxSizeBytes, MaxTimeMsNoEviction) ->
    ShouldEvict = should_evict(BucketName, MaxTimeMsNoEviction),

    DoEvict = fun () ->
                      T1 = sblob_util:now_fast(),
                      % TODO: remove iorio call here
                      TruncateResult = iorio:truncate(BucketName, MaxSizeBytes),
                      T2 = sblob_util:now_fast(),
                      TDiff = T2 - T1,
                      LogMsg = "bucket eviction ~s in ~pms ~p: ~p",
                      LogArgs = [BucketName, TDiff, Partition, TruncateResult],
                      if TDiff > 100 ->
                             lager:info(LogMsg, LogArgs);
                         true ->
                             lager:debug(LogMsg, LogArgs)
                      end,
                      set_last_eviction(BucketName)
              end,

    if ShouldEvict ->
           spawn(DoEvict),
           {ok, evicting};
       true ->
           lager:debug("no bucket eviction needed ~s ~p", [BucketName, Partition]),
           {ok, noaction}
    end.

