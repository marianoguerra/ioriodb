-module(iorio_config).
-export([stream_config/3, channel_config/2]).

-include_lib("sblob/include/sblob.hrl").

stream_config(Path, _Bucket, _Stream) ->
    GblobOpts = [{max_items, 4096}, {max_size_bytes, 4194304}],
    GblobServerOpts = [{check_interval_ms, 30000}, {max_interval_no_eviction_ms, 60000}],
    [{path, Path}, {gblob_opts, GblobOpts}, {gblob_server_opts, GblobServerOpts}].

channel_config(Bucket, Stream) ->
    BufferSize = application:get_env(iorio, channel_items_count, 50),
    ChName = <<Bucket/binary, <<"/">>/binary, Stream/binary>>,
    GetSeqNum = fun get_seqnum/1,
    [{buffer_size, BufferSize}, {get_seqnum, GetSeqNum}, {name, ChName}].

get_seqnum({entry, _Bucket, _Stream, #sblob_entry{seqnum=SeqNum}}) -> SeqNum.
