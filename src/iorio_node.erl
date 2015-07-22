-module(iorio_node).

-export([init/1, ping/1, put/5, coord_put_conditionally/8, coord_put/7,
         put_conditionally/6, get/6, subscribe/5, unsubscribe/4,
         evict_bucket/1, free_resources/1, truncate_percentage/4,
         bucket_size/3, list_buckets/2, list_streams/3, delete/1]).

-include_lib("sblob/include/sblob.hrl").
-include("include/iorio_node_state.hrl").

init(Opts) ->
    Partition = proplists:get_value(partition, Opts),
    lager:debug("partition init ~p", [Partition]),

    BasePath = iorio_bucket:base_path(),
    {ok, WriterPid} = iorio_vnode_writer:start_link(),

    PartitionStr = integer_to_list(Partition),
    Path = filename:join([BasePath, PartitionStr]),

    BucketsOpts = [{path, Path}, {partition, Partition}],

    {ok, Channels} = iorio_vnode_channels:start_link(),
    {ok, Buckets}  = iorio_vnode_buckets:start_link(BucketsOpts),

    % TODO: calculate based on number of buckets
    BucketEvictTimeInterval = application:get_env(iorio, bucket_evict_time_ms, 60000),

    Pid = self(),
    spawn(fun () ->
                  % distribute intervals to avoid calling all of them at once
                  % on the same node
                  {RandomSleep, _} = random:uniform_s(BucketEvictTimeInterval, now()),
                  timer:sleep(RandomSleep),
                  {ok, _TimerRef} = timer:send_interval(BucketEvictTimeInterval,
                                                        Pid, evict_bucket)
          end),


    State = #state{partition=Partition, path=Path, writer=WriterPid,
                   buckets=Buckets, channels=Channels},
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

truncate_percentage(State=#state{buckets=Buckets}, BucketName, Percentage, RefId) ->
    Result = iorio_vnode_buckets:truncate_percentage(Buckets, BucketName, Percentage),
    {reply, {RefId, Result}, State}.

bucket_size(State=#state{buckets=Buckets}, BucketName, RefId) ->
    Result = iorio_vnode_buckets:bucket_size(Buckets, BucketName),
    {reply, {RefId, Result}, State}.

list_buckets(State=#state{path=Path}, RefId) ->
    Buckets = iorio_bucket:list_buckets(Path),
    {reply, {RefId, Buckets}, State}.

list_streams(State=#state{path=Path}, Bucket, RefId) ->
    Streams = iorio_bucket:list_streams(Path, Bucket),
    {reply, {RefId, Streams}, State}.

delete(State=#state{path=Path}) ->
    iorio_bucket:delete(Path),
    {ok, State}.

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

free_resources(#state{buckets=Buckets, channels=Chans}) ->
    iorio_vnode_channels:clean(Chans),
    iorio_vnode_buckets:clean(Buckets).

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

