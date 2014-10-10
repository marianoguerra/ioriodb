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

-ignore_xref([
             start_vnode/1
             ]).

-record(state, {partition,
                path,
                next_bucket_index=1,
                max_bucket_time_no_evict_ms=60000,
                max_bucket_size_bytes=52428800,
                buckets,
                channels}).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    lager:debug("partition init ~p", [Partition]),
    CurrentDir = filename:absname("."),
    DefaultDataDir = filename:join([CurrentDir, "iorio_data"]),
    BasePath = application:get_env(iorio, data_path, DefaultDataDir),

    PartitionStr = integer_to_list(Partition),
    Path = filename:join([BasePath, PartitionStr]),
    Buckets = sblob_preg:new(),
    Channels = sblob_preg:new(),

    % TODO: configure and calculate based on number of buckets
    BucketEvictTimeInterval = 60000,

    Pid = self(),
    spawn(fun () ->
                  % distribute intervals to avoid calling all of them at once
                  % on the same node
                  Now = sblob_util:now_fast(),
                  {RandomSleep, _} = random:uniform_s(BucketEvictTimeInterval, now()),
                  timer:sleep(RandomSleep),
                  lager:debug("~p (~pms) setting interval for ~p",
                             [Now, RandomSleep, Partition]),
                  {ok, _TimerRef} = timer:send_interval(BucketEvictTimeInterval,
                                                        Pid, evict_bucket)
          end),

    {ok, #state{partition=Partition, path=Path, buckets=Buckets,
                channels=Channels}}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};

handle_command({put, ReqId, BucketName, Stream, Data}, _Sender, State) ->
    lager:debug("put ~p", [{ReqId, BucketName, Stream}]),
    Timestamp = sblob_util:now(),
    {State1, Entry} = do_put(State, BucketName, Stream, Timestamp, Data),
    {NewState, Channel} = get_channel(State1, BucketName, Stream),
    iorio_hist_channel:send(Channel, {entry, BucketName, Stream, Entry}),
    {reply, {ReqId, Entry}, NewState};

handle_command({get, BucketName, Stream, From, Count}, _Sender,
               State=#state{partition=Partition}) ->
    lager:debug("get ~s ~s ~p ~p at ~p", [BucketName, Stream, From, Count, Partition]),
    {NewState, Bucket} = get_bucket(State, BucketName),
    Entries = gblob_bucket:get(Bucket, Stream, From, Count),
    {reply, Entries, NewState};

handle_command({subscribe, BucketName, Stream, FromSeqNum, Pid}, _Sender, State=#state{partition=Partition}) ->
    lager:debug("subscribe ~s ~s at ~p", [BucketName, Stream, Partition]),
    {NewState, Channel} = get_channel(State, BucketName, Stream),
    iorio_hist_channel:subscribe(Channel, Pid, FromSeqNum),
    {reply, ok, NewState};

handle_command({unsubscribe, BucketName, Stream, Pid}, _Sender, State=#state{partition=Partition}) ->
    lager:debug("unsubscribe ~s ~s at ~p", [BucketName, Stream, Partition]),
    % TODO: don't create it if it doesn't exist
    {NewState, Channel} = get_channel(State, BucketName, Stream),
    iorio_hist_channel:unsubscribe(Channel, Pid),
    {reply, ok, NewState};

handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State=#state{partition=Partition}) ->
    lager:info("fold req ~p", [Partition]),
    Opts = [],
    GblobFoldFun = fun ({BucketName, StreamName,
                         #sblob_entry{seqnum=SeqNum, timestamp=Ts, data=Data}}, AccEntry) ->
                           Key = {BucketName, StreamName},
                           Val = {SeqNum, Ts, Data},
                           AccEntryOut = Fun(Key, Val, AccEntry),
                           {continue, AccEntryOut}
                   end,

    Acc = foldl_gblobs(State, fun({BucketName, Gblob=#gblob{name=StreamName}}, AccL) ->
                               lager:info("fold gblob ~s ~s", [BucketName, StreamName]),
                               Resp = gblob_util:fold(Gblob, Opts,
                                          fun (Entry, AccInner) ->
                                                  GblobFoldFun({BucketName, StreamName, Entry}, AccInner)
                                          end , AccL),
                               {_StopReason, AccLOut} = Resp,
                               AccLOut
                       end, Acc0),
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

handle_handoff_data(BinData, State) ->
    TermData = binary_to_term(BinData),
    lager:debug("handoff data received ~p", [TermData]),
    {{BucketName, StreamName}, {SeqNum, Ts, Data}} = TermData,
    {State1, Entry} = do_put(State, BucketName, StreamName, Ts, Data),
    GotSeqNum = Entry#sblob_entry.seqnum,
    if SeqNum =/= GotSeqNum ->
           lager:warning("seqnum mismatch on entry handoff expected ~p but got ~p",
                         [SeqNum, GotSeqNum]);
           true -> ok
    end,
    {reply, ok, State1}.

encode_handoff_item(Key, Value) ->
    term_to_binary({Key, Value}).

is_empty(State=#state{path=Path, partition=Partition}) ->
    IsEmpty = ((not filelib:is_dir(Path)) orelse
               (length(list_bucket_names(State)) == 0)),
    lager:info("handoff is empty? ~p ~p", [IsEmpty, Partition]),
    {IsEmpty, State}.

delete(State=#state{path=Path}) ->
    lager:info("handoff delete ~s", [Path]),
    sblob_util:remove(Path),
    {ok, State}.

handle_coverage({list_streams, Bucket}, _KeySpaces, {_, RefId, _}, State) ->
    Streams = list_gblobs_for_bucket(State, Bucket),
    StreamsBin = lists:map(fun list_to_binary/1, Streams),
    {reply, {RefId, StreamsBin}, State};

handle_coverage({list_buckets}, _KeySpaces, {_, RefId, _}, State) ->
    Buckets = list_bucket_names(State),
    {reply, {RefId, Buckets}, State};

handle_coverage({size, BucketName}, _KeySpaces, {_, RefId, _}, State=#state{path=Path}) ->
    HaveBucket = have_bucket(Path, BucketName),
    {NewState, Result} = if HaveBucket ->
                                {State1, Bucket} = get_bucket(State, BucketName),
                                SizeData = gblob_bucket:size(Bucket),
                                {State1, SizeData};
                            true -> {State, notfound}
                         end,
    {reply, {RefId, Result}, NewState};

handle_coverage({truncate_percentage, BucketName, Percentage}, _KeySpaces,
                {_, RefId, _}, State=#state{path=Path}) ->
    HaveBucket = have_bucket(Path, BucketName),
    {NewState, Result} = if HaveBucket ->
                                {State1, Bucket} = get_bucket(State, BucketName),
                                TResult = gblob_bucket:truncate_percentage(Bucket, Percentage),
                                {State1, TResult};
                            true -> {State, notfound}
                         end,
    {reply, {RefId, Result}, NewState};


handle_coverage(Req, _KeySpaces, _Sender, State) ->
    lager:warning("unknown coverage received ~p", [Req]),
    {noreply, State}.

handle_exit(_Pid, _Reason, State=#state{partition=Partition}) ->
    lager:info("handle exit ~p", [Partition]),
    State1 = free_resources(State),
    {noreply, State1}.

handle_info(evict_bucket, State=#state{partition=Partition,
                                       next_bucket_index=NextBucketIndex,
                                       max_bucket_time_no_evict_ms=MaxTimeMsNoEviction,
                                       max_bucket_size_bytes=MaxBucketSize}) ->
    BucketNames = lists:sort(list_bucket_names(State)),
    BucketCount = length(BucketNames),
    NewNextIndex = if
                       BucketCount == 0 ->
                           NextBucketIndex;
                       BucketCount > NextBucketIndex ->
                           1;
                       true ->
                           NextBucketIndex + 1
                   end,

    if NextBucketIndex > BucketCount ->
           lager:debug("no eviction, no buckets in vnode ~p", [Partition]),
           ok;
       true ->
           BucketName = lists:nth(NextBucketIndex, BucketNames),
           evict_bucket(BucketName, Partition, MaxBucketSize, MaxTimeMsNoEviction)
    end,

    {ok, State#state{next_bucket_index=NewNextIndex}}.

terminate(_Reason, State=#state{partition=Partition}) ->
    lager:info("terminate ~p", [Partition]),
    _State1 = free_resources(State),
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
                      _Result = iorio:truncate(BucketName, MaxSizeBytes),
                      T2 = sblob_util:now_fast(),
                      TDiff = T2 - T1,
                      lager:info("bucket eviction ~s in ~pms ~p",
                                 [BucketName, TDiff, Partition]),
                      set_last_eviction(BucketName)
              end,

    if ShouldEvict ->
           spawn(DoEvict),
        {ok, evicting};
       true ->
           lager:debug("no bucket eviction needed ~s ~p", [BucketName, Partition]),
           {ok, noaction}
    end.

free_resources(State=#state{buckets=Buckets}) ->
    EmptyBuckets = sblob_preg:new(),
    sblob_preg:foreach(Buckets, fun (_Key, Bucket) -> gblob_bucket:stop(Bucket) end),
    State1 = State#state{buckets=EmptyBuckets},
    State1.

have_bucket(Path, Bucket) ->
    BucketPath = filename:join([Path, Bucket]),
    filelib:is_dir(BucketPath).

list_dir(Path) ->
    case file:list_dir(Path) of
        {error, enoent} -> [];
        {ok, Names} -> Names
    end.

list_bucket_names(#state{path=Path}) ->
    lists:map(fun list_to_binary/1, list_dir(Path)).

list_gblobs_for_bucket(#state{path=Path}, BucketName) ->
    BucketPath = filename:join([Path, BucketName]),
    list_dir(BucketPath).

list_gblob_names(State=#state{path=Path}) ->
    BucketNames = list_bucket_names(State),
    lists:foldl(fun (BucketName, AccIn) ->
                        BucketPath = filename:join([Path, BucketName]),
                        GblobNames = list_dir(BucketPath),
                        lists:foldl(fun (StreamName, Items) ->
                                            StreamNameBin = list_to_binary(StreamName),
                                            [{BucketName, StreamNameBin}|Items]
                                    end, AccIn, GblobNames)
                end, [], BucketNames).


foldl_gblobs(State=#state{path=Path}, Fun, Acc0) ->
    GblobNames = list_gblob_names(State),
    lists:foldl(fun ({BucketName, GblobName}, AccIn) ->
                        BucketPath = filename:join([Path, BucketName, GblobName]),
                        Gblob = gblob:open(BucketPath, []),
                        AccOut = Fun({BucketName, Gblob}, AccIn),
                        gblob:close(Gblob),

                        AccOut
                end, Acc0, GblobNames).

get_bucket(State=#state{buckets=Buckets, path=Path}, BucketName) ->

    case sblob_preg:get(Buckets, BucketName) of
        none ->
            BucketNameStr = binary_to_list(BucketName),
            BucketPath = filename:join([Path, BucketNameStr]),
            GblobOpts = [],
            BucketOpts = [],
            {ok, Bucket} = gblob_bucket:start(BucketPath, GblobOpts, BucketOpts),
            NewBuckets = sblob_preg:put(Buckets, BucketName, Bucket),
            NewState = State#state{buckets=NewBuckets},

            {NewState, Bucket};
        {value, Bucket} ->
            {State, Bucket}
    end.

get_channel(State=#state{channels=Channels}, BucketName, Key) ->
    ChannelKey = {BucketName, Key},
    case sblob_preg:get(Channels, ChannelKey) of
        none ->
            % TODO: make it configurable
            BufferSize = 50,
            {ok, Channel} = iorio_hist_channel:new(BufferSize),
            NewChannels= sblob_preg:put(Channels, ChannelKey, Channel),
            NewState = State#state{channels=NewChannels},

            {NewState, Channel};
        {value, Channel} ->
            {State, Channel}
    end.

do_put(State=#state{partition=Partition}, BucketName, Stream, Timestamp, Data) ->
    lager:debug("put ~s ~s at ~p", [BucketName, Stream, Partition]),
    {NewState, Bucket} = get_bucket(State, BucketName),
    Entry = gblob_bucket:put(Bucket, Stream, Timestamp, Data),
    {NewState, Entry}.

