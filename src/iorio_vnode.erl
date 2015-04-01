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

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    lager:debug("partition init ~p", [Partition]),
    BasePath = base_path(),
    Opts = [{shard_lib_partition, Partition}, {base_dir, BasePath}],
    iorioc_shard:init(Opts).

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    iorioc_shard:ping(State);

handle_command({put, ReqId, BucketName, Stream, Timestamp, Data}, _Sender, State) ->
    iorioc_shard:put(State, ReqId, BucketName, Stream, Timestamp, Data);

handle_command({coord_put_conditionally, N, W, Bucket, Stream, Data, LastSeqNum, Pid},
               _Sender, State) ->
    Writer = iorioc_shard:writer(State),
    ReqID = make_ref(),
    Task = fun () ->
                   iorio_write_fsm:write_conditionally(N, W, Bucket, Stream,
                                                       Data, LastSeqNum,
                                                       Writer, ReqID),

                   receive {ReqID, _Val}=Result -> Pid ! Result end
           end,
    Writer ! Task,
    {reply, ReqID, State};

handle_command({coord_put, N, W, Bucket, Stream, Data, Pid}, _Sender, State) ->
    Writer = iorioc_shard:writer(State),
    ReqID = make_ref(),
    Task = fun () ->
                   iorio_write_fsm:write(N, W, Bucket, Stream, Data, Writer, ReqID),
                   receive {ReqID, _Val}=Result -> Pid ! Result end
           end,
    Writer ! Task,
    {reply, ReqID, State};

handle_command({put_conditionally, ReqId, BucketName, Stream, Ts, Data, LastSeqNum},
               _Sender, State) ->
    iorioc_shard:put_if_last(State, ReqId, BucketName, Stream, Ts, Data, LastSeqNum);

handle_command({get, BucketName, Stream, From, Count}, _Sender, State) ->
    % TODO: async
    iorioc_shard:get(State, BucketName, Stream, From, Count);

handle_command({subscribe, BucketName, Stream, FromSeqNum, Pid}, _Sender, State) ->
    iorioc_shard:subscribe(State, BucketName, Stream, FromSeqNum, Pid);

handle_command({unsubscribe, BucketName, Stream, Pid}, _Sender, State) ->
    iorioc_shard:unsubscribe(State, BucketName, Stream, Pid);

handle_command(Message, _Sender, State) ->
    lager:warning("Unhandled message ~p", [Message]),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Partition = iorioc_shard:partition(State),
    lager:info("fold req ~p", [Partition]),
    Opts = [],
    GblobFoldFun = fun ({BucketName, StreamName,
                         #sblob_entry{seqnum=SeqNum, timestamp=Ts, data=Data}}, AccEntry) ->
                           Key = {BucketName, StreamName},
                           Val = {SeqNum, Ts, Data},
                           AccEntryOut = Fun(Key, Val, AccEntry),
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
    Acc = iorioc_shard:foldl_gblobs(State, Fun, Acc0),
     {reply, Acc, State};

handle_handoff_command(Message, Sender, State) ->
    Partition = iorioc_shard:partition(State),
    lager:warning("handling command during handoff, state may diverge ~p",
                  [Partition]),
    handle_command(Message, Sender, State).

handoff_starting(_TargetNode, State) ->
    Partition = iorioc_shard:partition(State),
    lager:info("handoff starting ~p", [Partition]),
    {true, State}.

handoff_cancelled(State) ->
    Partition = iorioc_shard:partition(State),
    lager:info("handoff cancelled ~p", [Partition]),
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    Partition = iorioc_shard:partition(State),
    lager:info("handoff finished ~p", [Partition]),
    {ok, State}.

handle_handoff_data(BinData, State) ->
    TermData = binary_to_term(BinData),
    lager:debug("handoff data received ~p", [TermData]),
    {{BucketName, StreamName}, {SeqNum, Ts, Data}} = TermData,
    {State1, Entry} = iorioc_shard:put(State, make_ref(), BucketName,
                                       StreamName, Ts, Data, false),
    GotSeqNum = Entry#sblob_entry.seqnum,
    if SeqNum =/= GotSeqNum ->
           lager:warning("seqnum mismatch on entry handoff expected ~p but got ~p",
                         [SeqNum, GotSeqNum]);
           true -> ok
    end,
    {reply, ok, State1}.

encode_handoff_item(Key, Value) ->
    term_to_binary({Key, Value}).

is_empty(State) ->
    IsEmpty = iorioc_shard:is_empty(State),
    Partition = iorioc_shard:partition(State),
    lager:info("handoff is empty? ~p ~p", [IsEmpty, Partition]),
    {IsEmpty, State}.

delete(State) ->
    Partition = iorioc_shard:partition(State),
    iorioc_shard:delete(State),
    lager:info("handoff delete ~p", [Partition]),
    {ok, State}.

handle_coverage({list_streams, Bucket}, _KeySpaces, {_, RefId, _}, State) ->
    {reply, Streams, NewState} = iorioc_shard:list_streams(State, Bucket),
    {reply, {RefId, Streams}, NewState};

handle_coverage({list_buckets}, _KeySpaces, {_, RefId, _}, State) ->
    {reply, Buckets, NewState} = iorioc_chard:list_buckets(State),
    {reply, {RefId, Buckets}, NewState};

handle_coverage({size, BucketName}, _KeySpaces, {_, RefId, _}, State) ->
    {reply, Result, NewState} = iorioc_shard:bucket_size(State, BucketName),
    {reply, {RefId, Result}, NewState};

handle_coverage({truncate_percentage, _BucketName, _Percentage}, _KeySpaces,
                {_, RefId, _}, State) ->
    lager:warning("Truncate not implemented"),
    {reply, {RefId, notimplemented}, State};


handle_coverage(Req, _KeySpaces, _Sender, State) ->
    lager:warning("unknown coverage received ~p", [Req]),
    {noreply, State}.

handle_exit(_Pid, Reason, State) ->
    Partition = iorioc_shard:partition(State),
    lager:info("handle exit ~p ~p ~p", [Partition, Reason, State]),
    {noreply, State}.

handle_info(evict_bucket, State) ->
    EvictFun = fun evict_bucket/4,
    {_, NewState} = iorioc_shard:maybe_evict(State, EvictFun),
    {ok, NewState};

handle_info(Msg, State) ->
    lager:warning("unknown info received ~p", [Msg]),
    {ok, State}.

terminate(_Reason, State) ->
    Partition = iorioc_shard:partition(State),
    lager:info("terminate ~p", [Partition]),
    _State1 = iorioc_shard:stop(State),
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
                      TruncateResult = iorio:truncate(nil, BucketName, MaxSizeBytes),
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

base_path() ->
    CurrentDir = filename:absname("."),
    DefaultDataDir = filename:join([CurrentDir, "iorio_data"]),
    application:get_env(iorio, data_path, DefaultDataDir).

