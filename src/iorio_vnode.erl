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

    BasePath = iorio_anode:base_path(),

    PartitionStr = integer_to_list(Partition),
    Path = filename:join([BasePath, PartitionStr]),
    iorio_anode:new([{path, Path}, {partition, Partition}]).

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    to_reply(iorio_anode:ping(State));

handle_command({put, ReqId, BucketName, Stream, Data}, _Sender, State) ->
    to_reply(iorio_anode:put(State, ReqId, BucketName, Stream, Data));

handle_command({coord_put_conditionally, N, W, Bucket, Stream, Data, LastSeqNum, Pid},
               _Sender, State) ->
    Writer = iorio_anode:writer(State),
    ReqId = iorio_util:reqid(),
    iorio_vnode_writer:reply_to(Writer, Pid, ReqId,
                                iorio_write_fsm, write_conditionally,
                                [N, W, Bucket, Stream, Data, LastSeqNum,
                                 Writer, ReqId]),
    {reply, ReqId, State};

handle_command({coord_put, N, W, Bucket, Stream, Data, Pid}, _Sender, State) ->
    Writer = iorio_anode:writer(State),
    ReqId = iorio_util:reqid(),
    iorio_vnode_writer:reply_to(Writer, Pid, ReqId, iorio_write_fsm, write,
                                [N, W, Bucket, Stream, Data, Writer, ReqId]),
    {reply, ReqId, State};

handle_command({put_conditionally, ReqId, BucketName, Stream, Data, LastSeqNum},
               _Sender, State) ->
    to_reply(iorio_anode:put(State, ReqId, BucketName, Stream, Data, LastSeqNum));

handle_command({get, BucketName, Stream, From, Count}, Sender, State) ->
    Callback = fun (Entries) -> riak_core_vnode:reply(Sender, Entries) end,
    {_, NewState} = iorio_anode:get(State, BucketName, Stream, From, Count,
                                    Callback),
    {noreply, NewState};

handle_command({subscribe, BucketName, Stream, FromSeqNum, Pid}, _Sender, State) ->
    {_, NewState} = iorio_anode:subscribe(State, BucketName, Stream, FromSeqNum, Pid),
    {reply, ok, check(NewState, sub)};

handle_command({unsubscribe, BucketName, Stream, Pid}, _Sender, State) ->
    {_, NewState} = iorio_anode:unsubscribe(State, BucketName, Stream, Pid),
    {reply, ok, check(NewState, unsub)};

handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Partition = iorio_anode:partition(State),
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
    Acc = iorio_anode:foldl_gblobs(State, Fun, Acc0),
     {reply, Acc, State};

handle_handoff_command(Message, Sender, State) ->
    Partition = iorio_anode:partition(State),
    lager:warning("handling command during handoff, state may diverge ~p",
                  [Partition]),
    handle_command(Message, Sender, State).

handoff_starting(_TargetNode, State) ->
    Partition = iorio_anode:partition(State),
    lager:info("handoff starting ~p", [Partition]),
    {true, State}.

handoff_cancelled(State) ->
    Partition = iorio_anode:partition(State),
    lager:info("handoff cancelled ~p", [Partition]),
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    Partition = iorio_anode:partition(State),
    lager:info("handoff finished ~p", [Partition]),
    {ok, State}.

handle_handoff_data(BinData, State) ->
    TermData = binary_to_term(BinData),
    lager:debug("handoff data received ~p", [TermData]),
    {{BucketName, StreamName}, {SeqNum, Ts, Data}} = TermData,
    {Entry, State1} = iorio_anode:raw_put(State, BucketName, StreamName, Ts, Data),
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
    IsEmpty = iorio_anode:is_empty(State),
    Partition = iorio_anode:partition(State),
    lager:info("handoff is empty? ~p ~p", [IsEmpty, Partition]),
    {IsEmpty, State}.

delete(State) ->
    Partition = iorio_anode:partition(State),
    iorio_anode:delete(State),
    lager:info("handoff delete ~p", [Partition]),
    {ok, State}.

handle_coverage({list_streams, Bucket}, _KeySpaces, {_, RefId, _}, State) ->
    {Streams, NewState} = iorio_anode:list_streams(State, Bucket),
    {reply, {RefId, Streams}, NewState};

handle_coverage({list_buckets}, _KeySpaces, {_, RefId, _}, State) ->
    {Buckets, NewState} = iorio_anode:list_buckets(State),
    {reply, {RefId, Buckets}, NewState};

handle_coverage({size, BucketName}, _KeySpaces, {_, RefId, _}, State) ->
    HaveBucket = iorio_anode:have_bucket(State, BucketName),
    {NewState, Result} = if HaveBucket ->
                                {State1, Bucket} = iorio_anode:get_bucket(State, BucketName),
                                SizeData = gblob_bucket:size(Bucket),
                                {State1, SizeData};
                            true -> {State, notfound}
                         end,
    {reply, {RefId, Result}, NewState};

handle_coverage({truncate_percentage, BucketName, Percentage}, _KeySpaces,
                {_, RefId, _}, State) ->
    HaveBucket = iorio_anode:have_bucket(State, BucketName),
    {NewState, Result} = if HaveBucket ->
                                {State1, Bucket} = iorio_anode:get_bucket(State, BucketName),
                                TResult = gblob_bucket:truncate_percentage(Bucket, Percentage),
                                {State1, TResult};
                            true -> {State, notfound}
                         end,
    {reply, {RefId, Result}, NewState};


handle_coverage(Req, _KeySpaces, _Sender, State) ->
    lager:warning("unknown coverage received ~p", [Req]),
    {noreply, State}.

handle_exit(_Pid, _Reason, State) ->
    Partition = iorio_anode:partition(State),
    lager:info("handle exit ~p", [Partition]),
    State1 = iorio_anode:free_resources(State),
    {noreply, State1}.

handle_info(evict_bucket, State) ->
    EvictFun = fun evict_bucket/4,
    {_, NewState} = iorio_anode:maybe_evict(State, EvictFun),
    {ok, NewState};

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    NewState = iorio_anode:remove_bucket(State, Pid),
    {ok, NewState}.

terminate(Reason, State) ->
    Partition = iorio_anode:partition(State),
    lager:info("terminate ~p ~p", [Partition, Reason]),
    _State1 = iorio_anode:free_resources(State),
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

check(State=undefined, Place) ->
    lager:error("state is undefined! ~p", [Place]),
    State;
check(State, _Place) ->
    State.

to_reply({Result, NewState=undefined}) ->
    lager:error("state is undefined! ~p", [Result]),
    {reply, Result, NewState};
to_reply({Result, NewState}) ->
    {reply, Result, NewState}.
