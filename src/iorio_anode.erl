-module(iorio_anode).

-export([new/1, ping/1, put/5, put/6, raw_put/5, get/6, delete/1,
         list_buckets/1, list_streams/2, subscribe/5, unsubscribe/4]).

-export([writer/1, partition/1, is_empty/1, base_path/0, path/1,
        free_resources/1, have_bucket/2, foldl_gblobs/3, get_bucket/2,
        maybe_evict/2, remove_channel/2]).

-record(state, {partition,
                path,
                writer,
                next_bucket_index=1,
                max_bucket_time_no_evict_ms=60000,
                max_bucket_size_bytes=52428800,
                buckets_sup,
                buckets,
                channels_sup,
                channels}).

-include_lib("sblob/include/sblob.hrl").

new(Opts) ->
    {path, Path} = proplists:lookup(path, Opts),
    {partition, Partition} = proplists:lookup(partition, Opts),

    Buckets = sblob_preg:new(),
    Channels = sblob_preg:new(),

    {ok, BucketsSup} = gblob_buckets_sup:start_link(),
    {ok, ChannelsSup} = smc_channels_sup:start_link(),

    % XXX: have a supervisor?
    {ok, WriterPid} = iorio_vnode_writer:start_link(),

    % TODO: calculate based on number of buckets
    BucketEvictTimeInterval = application:get_env(iorio, bucket_evict_time_ms, 60000),
    MaxBucketSizeBytes = application:get_env(iorio, max_bucket_size_bytes, 52428800),

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
                channels=Channels, buckets_sup=BucketsSup,
                channels_sup=ChannelsSup,
                writer=WriterPid,
                max_bucket_size_bytes=MaxBucketSizeBytes}}.

base_path() ->
    CurrentDir = filename:absname("."),
    DefaultDataDir = filename:join([CurrentDir, "iorio_data"]),
    application:get_env(iorio, data_path, DefaultDataDir).

ping(State) ->
    {{pong, State#state.partition}, State}.

put(State, ReqId, BucketName, Stream, Data) ->
    put(State, ReqId, BucketName, Stream, Data, nil).

put(State, ReqId, BucketName, Stream, Data, LastSeqNum) ->
    lager:debug("put ~p", [{ReqId, BucketName, Stream}]),
    Info = get_put_info(State, BucketName, Stream),
    {Timestamp, Bucket, Channel, NewState} = Info,
    Reply = do_put(Bucket, BucketName, Stream, Timestamp, Data, ReqId, Channel,
                   LastSeqNum),
    {Reply, NewState}.

raw_put(State, BucketName, Stream, Timestamp, Data) ->
    {NewState, Bucket} = get_bucket(State, BucketName),
    Entry = gblob_bucket:put(Bucket, Stream, Timestamp, Data),
    {NewState, Entry}.

get(State=#state{partition=Partition}, BucketName, Stream, From, Count, Callback) ->
    lager:debug("get ~s ~s ~p ~p at ~p",
                [BucketName, Stream, From, Count, Partition]),
    {NewState, Bucket} = get_bucket(State, BucketName),
    Result = gblob_bucket:get_cb(Bucket, Stream, From, Count, Callback),
    {Result, NewState}.

subscribe(State=#state{partition=Partition}, BucketName, Stream, FromSeqNum, Pid) ->
    lager:debug("subscribe ~s ~s at ~p", [BucketName, Stream, Partition]),
    {NewState, Channel} = get_channel(State, BucketName, Stream),
    check_channel(Channel),

    try
        smc_hist_channel:replay(Channel, Pid, FromSeqNum),
        Result = smc_hist_channel:subscribe(Channel, Pid),
        {Result, NewState}
    catch T:E ->
              lager:error("Error subscribing to channel ~p/~p ~p ~p",
                          [BucketName, Stream, T, error_info(E),
                           erlang:is_process_alive(Channel)]),
        {error, NewState}
    end.

unsubscribe(State=#state{partition=Partition}, BucketName, Stream, Pid) ->
    lager:debug("unsubscribe ~s ~s at ~p", [BucketName, Stream, Partition]),
    case get_existing_channel(State, BucketName, Stream) of
        {some, Channel} ->
            check_channel(Channel),

            try
                Result = smc_hist_channel:unsubscribe(Channel, Pid),
                {Result, State}
            catch T:E ->
                      lager:error("Error unsubscribing to channel ~p/~p ~p ~p",
                                  [BucketName, Stream, T, error_info(E),
                                   erlang:is_process_alive(Channel)]),
                      {{error, {T, E}}, State}
            end;
        none ->
            lager:warning("unsubscribing for inexistent channel ~p/~p",
                          [BucketName, Stream]),
            {{error, {no_channel, {BucketName, Stream}}}, State}
    end.

writer(#state{writer=Writer}) -> Writer.
partition(#state{partition=Partition}) -> Partition.
path(#state{path=Path}) -> Path.

is_empty(State=#state{path=Path}) ->
    (not filelib:is_dir(Path)) orelse (length(list_bucket_names(State)) == 0).

delete(#state{path=Path}) ->
    sblob_util:remove(Path).

list_streams(State, Bucket) ->
    Streams = list_gblobs_for_bucket(State, Bucket),
    StreamsBin = lists:map(fun list_to_binary/1, Streams),
    {StreamsBin, State}.

list_buckets(State) ->
    Buckets = list_bucket_names(State),
    {Buckets, State}.

free_resources(State=#state{buckets=Buckets}) ->
    EmptyBuckets = sblob_preg:new(),
    sblob_preg:foreach(Buckets, fun (_Key, Bucket) ->
                                        gblob_bucket:stop(Bucket)
                                end),
    State#state{buckets=EmptyBuckets}.

get_bucket(State=#state{buckets=Buckets, path=Path, buckets_sup=BucketsSup}, BucketName) ->

    case sblob_preg:get(Buckets, BucketName) of
        none ->
            BucketNameStr = binary_to_list(BucketName),
            BucketPath = filename:join([Path, BucketNameStr]),
            GblobOpts = [],
            BucketOpts = [],
            {ok, Bucket} = gblob_buckets_sup:start_child(BucketsSup,
                                                         [BucketPath, GblobOpts, BucketOpts]),
            NewBuckets = sblob_preg:put(Buckets, BucketName, Bucket),
            erlang:monitor(process, Bucket),
            NewState = State#state{buckets=NewBuckets},

            {NewState, Bucket};
        {value, Bucket} ->
            {State, Bucket}
    end.

maybe_evict(State=#state{partition=Partition,
                         next_bucket_index=NextBucketIndex,
                         max_bucket_time_no_evict_ms=MaxTimeMsNoEviction,
                         max_bucket_size_bytes=MaxBucketSize},
            EvictFun) ->

    BucketNames = lists:sort(list_bucket_names(State)),
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

remove_channel(State=#state{buckets=Buckets, channels=Channels}, Pid) ->
    % TODO: don't do it like this, create one process for each to handle the
    % DOWN events
    NewBuckets = sblob_preg:remove_reverse(Buckets, Pid),
    NewChannels = sblob_preg:remove_reverse(Channels, Pid),
    State#state{buckets=NewBuckets, channels=NewChannels}.

%% Private

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

get_seqnum({entry, _Bucket, _Stream, #sblob_entry{seqnum=SeqNum}}) -> SeqNum.

get_existing_channel(State=#state{channels=Channels, channels_sup=ChannelsSup}, BucketName, Key) ->
    ChannelKey = {BucketName, Key},
    case sblob_preg:get(Channels, ChannelKey) of
        none -> none;
        {value, Channel} -> {some, Channel}
    end.

get_channel(State=#state{channels=Channels, channels_sup=ChannelsSup}, BucketName, Key) ->
    ChannelKey = {BucketName, Key},
    case sblob_preg:get(Channels, ChannelKey) of
        none ->
            % TODO: make it configurable
            BufferSize = 50,
            GetSeqNum = fun get_seqnum/1,
            ChOpts = [{buffer_size, BufferSize}, {get_seqnum, GetSeqNum}],
            {ok, Channel} = smc_channels_sup:start_child(ChannelsSup, ChOpts),
            erlang:monitor(process, Channel),
            NewChannels= sblob_preg:put(Channels, ChannelKey, Channel),
            NewState = State#state{channels=NewChannels},

            {NewState, Channel};
        {value, Channel} ->
            {State, Channel}
    end.

get_put_info(State, BucketName, Stream) ->
    Timestamp = sblob_util:now(),
    {State1, Bucket} = get_bucket(State, BucketName),
    {NewState, Channel} = get_channel(State1, BucketName, Stream),
    check_channel(Channel),
    {Timestamp, Bucket, Channel, NewState}.

do_put(Bucket, BucketName, Stream, Timestamp, Data, ReqId, Channel, LastSeqNum) ->

    Result = if
        LastSeqNum == nil ->
            gblob_bucket:put(Bucket, Stream, Timestamp, Data);
        true ->
            gblob_bucket:put(Bucket, Stream, Timestamp, Data, LastSeqNum)
    end,

    case Result of
        {error, _Reason}=Error ->
            {ReqId, Error};
        Entry ->
            WasAlive = erlang:is_process_alive(Channel),

            if WasAlive -> ok;
               true -> lager:warning("Channel is dead ~p ~p", [Channel, WasAlive])
            end,

            try
                smc_hist_channel:send(Channel, {entry, BucketName, Stream, Entry})
            catch T:E ->
                IsAlive = erlang:is_process_alive(Channel),
                lager:error("Error sending event to channel ~p/~p ~p ~p ~p alive: ~p/~p",
                            [BucketName, Stream, T, error_info(E), Channel, WasAlive, IsAlive])
            end,
            {ReqId, Entry}
    end.

error_info({noproc, {gen_server, call, _}}) ->
    {noproc, {gen_server, call}};
error_info(Other) ->
    Other.


have_bucket(#state{path=Path}, Bucket) ->
    BucketPath = filename:join([Path, Bucket]),
    filelib:is_dir(BucketPath).

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


foldl_gblobs(State, Fun, Acc0) ->
    Path = iorio_anode:path(State),
    GblobNames = list_gblob_names(State),
    lists:foldl(fun ({BucketName, GblobName}, AccIn) ->
                        BucketPath = filename:join([Path, BucketName, GblobName]),
                        Gblob = gblob:open(BucketPath, []),
                        AccOut = Fun({BucketName, Gblob}, AccIn),
                        gblob:close(Gblob),

                        AccOut
                end, Acc0, GblobNames).

check_channel(Channel) ->
    IsAlive = erlang:is_process_alive(Channel),

    if IsAlive -> ok;
       true -> lager:warning("got dead channel ~p: ~p", [Channel, IsAlive])
    end.

