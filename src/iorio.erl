-module(iorio).
-include("iorio.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/1, put/4, put/7, put_conditionally/8, get/4, get/5, get_last/3,
         subscribe/4, subscribe/5, unsubscribe/4, list/1, list/2, list/3,
         bucket_size/2, bucket_size/3, truncate/3, truncate_percentage/3]).

get_index_node(Bucket, Stream) ->
    DocIdx = riak_core_util:chash_key({Bucket, Stream}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, iorio),
    [{IndexNode, _Type}] = PrefList,
    IndexNode.

-define(DEFAULT_TIMEOUT_MS, 5000).
-define(DEFAULT_N, 3).
-define(DEFAULT_W, 3).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping(_State) ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, iorio),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, iorio_vnode_master).

put(State, Bucket, Stream, Data) ->
    put(State, Bucket, Stream, Data, ?DEFAULT_N, ?DEFAULT_W, ?DEFAULT_TIMEOUT_MS).

put(_State, Bucket, Stream, Data, N, W, Timeout) ->
    IndexNode = get_index_node(Bucket, Stream),
    Pid = self(),
    Args = {coord_put, N, W, Bucket, Stream, Data, Pid},
    ReqID = riak_core_vnode_master:sync_spawn_command(IndexNode, Args, iorio_vnode_master),
    wait_for_reqid(ReqID, Timeout).

put_conditionally(_State, Bucket, Stream, Data, LastSeqNum, N, W, Timeout) ->
    IndexNode = get_index_node(Bucket, Stream),
    Pid = self(),
    Args = {coord_put_conditionally, N, W, Bucket, Stream, Data, LastSeqNum, Pid},
    ReqID = riak_core_vnode_master:sync_spawn_command(IndexNode, Args, iorio_vnode_master),
    wait_for_reqid(ReqID, Timeout).

get_last(_State, Bucket, Stream) ->
    case get(Bucket, Stream, nil, 1) of
        [Blob] -> {ok, Blob};
        [] -> notfound
    end.

get(_State, Bucket, Stream, From) ->
    get(Bucket, Stream, From, 1).

get(_State, Bucket, Stream, From, Count) ->
    IndexNode = get_index_node(Bucket, Stream),
    riak_core_vnode_master:sync_spawn_command(IndexNode,
                                              {get, Bucket, Stream, From, Count},
                                              iorio_vnode_master).

subscribe(_State, Bucket, Stream, Pid) ->
    subscribe(Bucket, Stream, nil, Pid).

subscribe(_State, Bucket, Stream, FromSeqNum, Pid) ->
    IndexNode = get_index_node(Bucket, Stream),
    riak_core_vnode_master:sync_spawn_command(IndexNode,
                                              {subscribe, Bucket, Stream, FromSeqNum, Pid},
                                              iorio_vnode_master).

unsubscribe(_State, Bucket, Stream, Pid) ->
    IndexNode = get_index_node(Bucket, Stream),
    riak_core_vnode_master:sync_spawn_command(IndexNode,
                                              {unsubscribe, Bucket, Stream, Pid},
                                              iorio_vnode_master).

list(_State) ->
    Timeout = 5000,
    iorio_coverage_fsm:start({list_buckets}, Timeout).

list(_State, Bucket) ->
    list(Bucket, ?DEFAULT_TIMEOUT_MS).

list(_State, Bucket, Timeout) ->
    iorio_coverage_fsm:start({list_streams, Bucket}, Timeout).

bucket_size(_State, Bucket) ->
    bucket_size(Bucket, ?DEFAULT_TIMEOUT_MS).

bucket_size(_State, Bucket, Timeout) ->
    Result = case iorio_coverage_fsm:start({size, Bucket}, Timeout) of
                 {ok, Data} -> Data;
                 {partial, Reason, PartialData} ->
                     lager:warning("partial data getting bucket size ~p: ~p",
                                   [Bucket, Reason]),
                     PartialData
             end,
    AllSizes = filter_notfound(Result),

    NamesAndSizes = lists:flatmap(fun ({_VNode, _Node, {_TotalSize, Streams}}) ->
                                          Streams
                                  end, AllSizes),

    SortedStreamSizes = lists:sort(fun ({Name1, Size1}, {Name2, Size2}) ->
                        % TODO: check this, we want to sort by same stream name
                        % first and then from bigger sizes to smaller ones
                        if Name1 < Name2 -> true;
                           Name1 == Name2 -> Size1 >= Size2;
                           true -> false
                        end
                end, NamesAndSizes),

    StreamSizes = lists:usort(fun ({Name1, _Size1}, {Name2, _Size2}) ->
                                      Name1 =< Name2
                              end, SortedStreamSizes),

    TotalSize = lists:foldl(fun ({_Name, Size}, CurTotal) ->
                                    Size + CurTotal
                            end, 0, StreamSizes),
    {TotalSize, StreamSizes}.

truncate_percentage(_State, Bucket, Percentage) ->
    Timeout = ?DEFAULT_TIMEOUT_MS,
    lager:info("truncating bucket ~s to ~p%", [Bucket, Percentage * 100]),
    Result = iorio_coverage_fsm:start({truncate_percentage, Bucket, Percentage},
                                      Timeout),
    case Result of
        {ok, Data} ->
            {ok, filter_notfound(Data)};
        {partial, Reason, PartialData} ->
            {partial, Reason, filter_notfound(PartialData)}
    end.

truncate(State, Bucket, MaxSizeBytes) ->
    {TotalSizeBytes, _} = bucket_size(State, Bucket),
    if TotalSizeBytes > MaxSizeBytes ->
           Percentage = (MaxSizeBytes * 0.5) / TotalSizeBytes,
           truncate_percentage(State, Bucket, Percentage);
       true -> {ok, noaction}
    end.

%% private
wait_for_reqid(ReqID, Timeout) ->
    receive
        {ReqID, {error, Reason}} -> {error, Reason};
        {ReqID, Val} -> {ok, Val}
    after Timeout -> {error, timeout}
    end.

filter_notfound(Items) ->
    lists:filter(fun ({_VNode, _Node, notfound}) -> false;
                     (_) -> true
                 end, Items).
