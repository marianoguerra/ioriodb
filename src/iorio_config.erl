-module(iorio_config).
-export([stream_config/3, channel_config/2, put/3, get/2, get/3, merge/3]).

-export([unpack/2]).

-include_lib("sblob/include/sblob.hrl").

-define(FULLPREFIX, {<<"iorio">>, <<"config">>}).

put(Section, Key, Value) ->
    riak_core_metadata:put(?FULLPREFIX, {Section, Key}, Value).

get(Section, Key) -> get(Section, Key, undefined).

get(Section, Key, Default) ->
    riak_core_metadata:get(?FULLPREFIX, {Section, Key}, [{default, Default}]).

merge(Section, Key, Values) ->
    Current = get(Section, Key, []),
    NewValues = lists:ukeymerge(1, lists:ukeysort(1, Current),
                                   lists:ukeysort(1, Values)),

    put(Section, Key, NewValues).

stream_config(Path, Bucket, Stream) ->
    StreamCfg = get(stream, {Bucket, Stream}, []),
    [GblobOpts0, GblobServerOpts0] = unpack(StreamCfg, [gblob, gblob_server]),

    MaxItems = env(gblob_max_items, 4096),
    MaxSizeBytes = env(gblob_max_size_bytes, 4194304),
    CheckIntervalMs = env(gblob_check_interval_ms, 30000),
    MaxIntNoEvictionMs = env(gblob_max_interval_no_eviction_ms, 60000),


    GblobOptsDefaults = [{max_items, MaxItems}, {max_size_bytes, MaxSizeBytes}],
    GblobServerOptsDefaults = [{check_interval_ms, CheckIntervalMs},
                               {max_interval_no_eviction_ms, MaxIntNoEvictionMs}],

    GblobOpts = GblobOpts0 ++ GblobOptsDefaults,
    GblobServerOpts = GblobServerOpts0 ++ GblobServerOptsDefaults,

    [{path, Path}, {gblob_opts, GblobOpts},
     {gblob_server_opts, GblobServerOpts}].

channel_config(Bucket, Stream) ->
    BufferSize = env(channel_items_count, 50),
    MaxSizeBytes = env(channel_items_size, 1048576),
    ChName = <<Bucket/binary, <<"/">>/binary, Stream/binary>>,
    GetSeqNum = fun get_seqnum/1,
    [{buffer_size, BufferSize}, {get_seqnum, GetSeqNum}, {name, ChName},
     {buffer_max_size_bytes, MaxSizeBytes}].

get_seqnum({entry, _Bucket, _Stream, #sblob_entry{seqnum=SeqNum}}) -> SeqNum.

% private

unpack(PList, Keys) -> unpack(PList, Keys, [], [], []).

% PList, Keys, KeyAccum, Accum, RemPList
unpack([], [], [], Accum, _RemPList) ->
    lists:reverse(Accum);
unpack([], [_Key|Keys], KeyAccum, Accum, RemPList) ->
    unpack(RemPList, Keys, [], [KeyAccum|Accum], []);
unpack([{Key, Val={_SubKey, _Val}}|PList], [Key|_]=Keys, KeyAccum, Accum,
       RemPList) ->
    unpack(PList, Keys, [Val|KeyAccum], Accum, RemPList);
unpack([Val|PList], Keys, KeyAccum, Accum, RemPList) ->
    unpack(PList, Keys, KeyAccum, Accum, [Val|RemPList]).

env(Key, Default) ->
    application:get_env(iorio, Key, Default).
