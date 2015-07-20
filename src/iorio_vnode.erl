-module(iorio_vnode).
-behaviour(riak_core_vnode).

-include("iorio.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include_lib("sblob/include/sblob.hrl").
-include_lib("sblob/include/gblob.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         handle_info/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-ignore_xref([start_vnode/1, handle_info/2]).

-record(state, {path, partition, writer, buckets, channels}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
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

handle_command(ping, _Sender, State=#state{partition=Partition}) ->
    {reply, {pong, Partition}, State};

handle_command({put, ReqId, BucketName, Stream, Data},
               _Sender, State=#state{buckets=Buckets, channels=Channels}) ->
    Reply = iorio_vnode_buckets:put(Buckets, ReqId, BucketName, Stream, Data),
    send_to_channel(Reply, Channels, BucketName, Stream),
    {reply, Reply, State};

handle_command({coord_put_conditionally, N, W, Bucket, Stream, Data, LastSeqNum, Pid},
               _Sender, State=#state{writer=Writer}) ->
    ReqId = iorio_util:reqid(),
    iorio_vnode_writer:reply_to(Writer, Pid, ReqId,
                                iorio_write_fsm, write_conditionally,
                                [N, W, Bucket, Stream, Data, LastSeqNum,
                                 Writer, ReqId]),
    {reply, ReqId, State};

handle_command({coord_put, N, W, Bucket, Stream, Data, Pid},
               _Sender, State=#state{writer=Writer}) ->
    ReqId = iorio_util:reqid(),
    iorio_vnode_writer:reply_to(Writer, Pid, ReqId, iorio_write_fsm, write,
                                [N, W, Bucket, Stream, Data, Writer, ReqId]),
    {reply, ReqId, State};

handle_command({put_conditionally, ReqId, BucketName, Stream, Data, LastSeqNum},
               _Sender, State=#state{buckets=Buckets, channels=Channels}) ->
    Reply = iorio_vnode_buckets:put(Buckets, ReqId, BucketName, Stream, Data, LastSeqNum),
    send_to_channel(Reply, Channels, BucketName, Stream),
    {reply, Reply, State};

handle_command({get, BucketName, Stream, From, Count},
               Sender, State=#state{buckets=Buckets}) ->
    Callback = fun (Entries) -> riak_core_vnode:reply(Sender, Entries) end,
    iorio_vnode_buckets:get(Buckets, BucketName, Stream, From, Count, Callback),
    {noreply, State};

handle_command({subscribe, BucketName, Stream, FromSeqNum, Pid},
               _Sender, State=#state{channels=Channels}) ->
    iorio_vnode_channels:subscribe(Channels, BucketName, Stream, FromSeqNum, Pid),
    {reply, ok, State};

handle_command({unsubscribe, BucketName, Stream, Pid}, _Sender,
               State=#state{channels=Channels}) ->
    iorio_vnode_channels:unsubscribe(Channels, BucketName, Stream, Pid),
    {reply, ok, State};

handle_command(Message, _Sender, State) ->
    lager:warning("unknown command ~p", [Message]),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=FoldFun, acc0=Acc0}, _Sender,
                       State=#state{partition=Partition, path=Path}) ->
    lager:info("fold req ~p", [Partition]),
    Opts = [],
    GblobFoldFun = fun ({BucketName, StreamName,
                         #sblob_entry{seqnum=SeqNum, timestamp=Ts, data=Data}}, AccEntry) ->
                           Key = {BucketName, StreamName},
                           Val = {SeqNum, Ts, Data},
                           AccEntryOut = FoldFun(Key, Val, AccEntry),
                           {continue, AccEntryOut}
                   end,

    Fun = fun({BucketName, Gblob=#gblob{name=StreamName}}, AccL) ->
                  lager:info("fold gblob ~s ~s", [BucketName, StreamName]),
                  FunI = fun (Entry, AccInner) ->
                                 EntryData = {BucketName, StreamName, Entry},
                                 GblobFoldFun(EntryData, AccInner)
                         end,
                  Resp = gblob_util:fold(Gblob, Opts, FunI, AccL),
                  {_StopReason, AccLOut} = Resp,
                  AccLOut
          end,
    Acc = iorio_bucket:foldl_gblobs(Path, Fun, Acc0),
    {reply, Acc, State};

handle_handoff_command(Message, Sender, State=#state{partition=Partition}) ->
    lager:warning("handling command during handoff, state may diverge ~p",
                  [Partition]),
    handle_command(Message, Sender, State).

handoff_starting(_TargetNode, State=#state{partition=Partition}) ->
    lager:info("handoff starting ~p", [Partition]),
    {true, State}.

handoff_cancelled(State=#state{partition=Partition}) ->
    lager:info("handoff cancelled ~p", [Partition]),
    {ok, State}.

handoff_finished(_TargetNode, State=#state{partition=Partition}) ->
    lager:info("handoff finished ~p", [Partition]),
    {ok, State}.

handle_handoff_data(BinData, State=#state{buckets=Buckets}) ->
    TermData = binary_to_term(BinData),
    lager:debug("handoff data received ~p", [TermData]),
    {{BucketName, StreamName}, {SeqNum, Ts, Data}} = TermData,
    Entry = iorio_vnode_buckets:raw_put(Buckets, BucketName, StreamName, Ts, Data),
    GotSeqNum = Entry#sblob_entry.seqnum,
    if SeqNum =/= GotSeqNum ->
           lager:warning("seqnum mismatch on entry handoff expected ~p but got ~p",
                         [SeqNum, GotSeqNum]);
       true -> ok
    end,
    {reply, ok, State}.

encode_handoff_item(Key, Value) ->
    term_to_binary({Key, Value}).

is_empty(State=#state{path=Path, partition=Partition}) ->
    IsEmpty = iorio_bucket:is_empty(Path),
    lager:info("handoff is empty? ~p ~p", [IsEmpty, Partition]),
    {IsEmpty, State}.

delete(State=#state{path=Path, partition=Partition}) ->
    iorio_bucket:delete(Path),
    lager:info("handoff delete ~p", [Partition]),
    {ok, State}.

handle_coverage({list_streams, Bucket}, _KeySpaces, {_, RefId, _}, State=#state{path=Path}) ->
    Streams = iorio_bucket:list_streams(Path, Bucket),
    {reply, {RefId, Streams}, State};

handle_coverage({list_buckets}, _KeySpaces, {_, RefId, _}, State=#state{path=Path}) ->
    Buckets = iorio_bucket:list_buckets(Path),
    {reply, {RefId, Buckets}, State};

handle_coverage({size, BucketName}, _KeySpaces, {_, RefId, _},
                State=#state{buckets=Buckets}) ->
    Result = iorio_vnode_buckets:bucket_size(Buckets, BucketName),
    {reply, {RefId, Result}, State};

handle_coverage({truncate_percentage, BucketName, Percentage}, _KeySpaces,
                {_, RefId, _}, State=#state{buckets=Buckets}) ->
    Result = iorio_vnode_buckets:truncate_percentage(Buckets, BucketName, Percentage),
    {reply, {RefId, Result}, State};


handle_coverage(Req, _KeySpaces, _Sender, State) ->
    lager:warning("unknown coverage received ~p", [Req]),
    {noreply, State}.

handle_exit(_Pid, _Reason, State=#state{partition=Partition}) ->
    lager:info("handle exit ~p", [Partition]),
    free_resources(State),
    {noreply, State}.

handle_info(evict_bucket, State=#state{buckets=Buckets}) ->
    EvictFun = fun evict_bucket/4,
    iorio_vnode_buckets:check_evict(Buckets, EvictFun),
    {ok, State};

handle_info(Msg, State=#state{partition=Partition}) ->
    lager:warning("Unexpected handle info msg: ~p ~p", [Partition, Msg]),
    {ok, State}.

terminate(Reason, State=#state{partition=Partition}) ->
    lager:info("terminate ~p ~p", [Partition, Reason]),
    free_resources(State),
    ok.

%% private api

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

free_resources(#state{buckets=Buckets, channels=Chans}) ->
    iorio_vnode_channels:clean(Chans),
    iorio_vnode_buckets:clean(Buckets).

send_to_channel({_ReqId, {error, _Reason}}, _Channels, _BucketName, _Stream) ->
    ok;
send_to_channel({_ReqId, Entry=#sblob_entry{}}, Channels, BucketName, Stream) ->
    iorio_vnode_channels:send(Channels, BucketName, Stream, Entry).
