-module(iorio).
-include("iorio.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0, put/3, get/3, get/4, subscribe/3, unsubscribe/3, list/0,
         list/1, list/2, size/1, size/2]).

-ignore_xref([ping/0]).

get_index_node(Bucket, Stream) ->
    DocIdx = riak_core_util:chash_key({Bucket, Stream}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, iorio),
    [{IndexNode, _Type}] = PrefList,
    IndexNode.

-define(DEFAULT_TIMEOUT_MS, 5000).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, iorio),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, iorio_vnode_master).

put(Bucket, Stream, Data) ->
    % TODO: make them configurable
    N = 3,
    W = 3,
    Timeout = 5000,

    {ok, ReqID} = iorio_write_fsm:write(N, W, Bucket, Stream, Data),
    wait_for_reqid(ReqID, Timeout).

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

list() ->
    Timeout = 5000,
    iorio_coverage_fsm:start({list_buckets}, Timeout).

list(Bucket) ->
    list(Bucket, ?DEFAULT_TIMEOUT_MS).

list(Bucket, Timeout) ->
    iorio_coverage_fsm:start({list_streams, Bucket}, Timeout).

size(Bucket) ->
    size(Bucket, ?DEFAULT_TIMEOUT_MS).

size(Bucket, Timeout) ->
    Result = case iorio_coverage_fsm:start({size, Bucket}, Timeout) of
                 {ok, Data} -> Data;
                 {partial, Reason, PartialData} ->
                     lager:warning("partial data getting bucket size ~p: ~p",
                                   [Bucket, Reason]),
                     PartialData
             end,
    AllSizes = lists:filter(fun ({_VNode, _Node, notfound}) -> false;
                     (_) -> true
                 end, Result),

    NamesAndSizes = lists:flatmap(fun ({_VNode, _Node, {_TotalSize, Streams}}) ->
                                          Streams
                                  end, AllSizes),

    StreamSizes = lists:usort(fun ({Name1, Size1}, {Name2, Size2}) ->
                        % TODO: check this
                        if Name1 < Name2 -> true;
                           Name1 == Name2 -> Size1 =< Size2;
                           true -> false
                        end
                end, NamesAndSizes),
    TotalSize = lists:foldl(fun ({_Name, Size}, CurTotal) ->
                                    Size + CurTotal
                            end, 0, StreamSizes),
    {TotalSize, StreamSizes}.

%% private
wait_for_reqid(ReqID, Timeout) ->
    receive {ReqID, Val} -> {ok, Val}
    after Timeout -> {error, timeout}
    end.

