-module(iorio).
-include("iorio.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0, put/3, get/3, get/4, subscribe/3, unsubscribe/3]).

-ignore_xref([ping/0]).

get_index_node(Bucket, Stream) ->
    DocIdx = riak_core_util:chash_key({Bucket, Stream}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, iorio),
    [{IndexNode, _Type}] = PrefList,
    IndexNode.

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, iorio),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, iorio_vnode_master).

put(Bucket, Stream, Data) ->
    IndexNode = get_index_node(Bucket, Stream),
    riak_core_vnode_master:sync_spawn_command(IndexNode,
                                              {put, Bucket, Stream, Data},
                                              iorio_vnode_master).

get(Bucket, Stream, From) ->
    get(Bucket, Stream, From, 1).

get(Bucket, Stream, From, Count) ->
    IndexNode = get_index_node(Bucket, Stream),
    riak_core_vnode_master:sync_spawn_command(IndexNode,
                                              {get, Bucket, Stream, From, Count},
                                              iorio_vnode_master).

subscribe(Bucket, Stream, Pid) ->
    IndexNode = get_index_node(Bucket, Stream),
    riak_core_vnode_master:sync_spawn_command(IndexNode,
                                              {subscribe, Bucket, Stream, Pid},
                                              iorio_vnode_master).

unsubscribe(Bucket, Stream, Pid) ->
    IndexNode = get_index_node(Bucket, Stream),
    riak_core_vnode_master:sync_spawn_command(IndexNode,
                                              {unsubscribe, Bucket, Stream, Pid},
                                              iorio_vnode_master).
