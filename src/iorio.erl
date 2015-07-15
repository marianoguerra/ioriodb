-module(iorio).
-include("iorio.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0, put/3, put/6, put_conditionally/7, get/3, get/4, get_last/2,
         subscribe/3, subscribe/4, unsubscribe/3, list/1, list/2, list/3,
         bucket_size/1, bucket_size/2, truncate/2, truncate_percentage/2,
         init/0, stats/0]).

-ignore_xref([ping/0, bucket_size/1, bucket_size/2, get/3, list/3, put/3,
              subscribe/3, subscribe/4, truncate_percentage/2, unsubscribe/3]).

get_index_node(Bucket, Stream) ->
    DocIdx = riak_core_util:chash_key({Bucket, Stream}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, iorio),
    [{IndexNode, _Type}] = PrefList,
    IndexNode.

-define(DEFAULT_TIMEOUT_MS, 5000).
-define(DEFAULT_N, 3).
-define(DEFAULT_W, 3).

%% Public API

init() ->
    case iorio_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, iorio_vnode}]),

            ok = riak_core_ring_events:add_guarded_handler(iorio_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(iorio_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(iorio, self()),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stats() ->
    Stats = riak_core_stat:get_stats(),
    KeyToString = fun ({K, V}) ->
                          StrKeyTokens = lists:map(fun to_string/1, tl(tl(K))),
                          StrKey = string:join(StrKeyTokens, "_"),
                          BinKey = list_to_binary(StrKey),
                          {BinKey, V}
                  end,
    lists:map(KeyToString, Stats).

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    iorio_stats:core_ping(),
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, iorio),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, iorio_vnode_master).

put(Bucket, Stream, Data) ->
    put(Bucket, Stream, Data, ?DEFAULT_N, ?DEFAULT_W, ?DEFAULT_TIMEOUT_MS).

put(Bucket, Stream, Data, N, W, Timeout) ->
    iorio_stats:core_put(),
    iorio_stats:core_msg_size(size(Data)),
    IndexNode = get_index_node(Bucket, Stream),
    Pid = self(),
    Args = {coord_put, N, W, Bucket, Stream, Data, Pid},
    ReqID = riak_core_vnode_master:sync_spawn_command(IndexNode, Args, iorio_vnode_master),
    wait_for_reqid(ReqID, Timeout).

put_conditionally(Bucket, Stream, Data, LastSeqNum, N, W, Timeout) ->
    IndexNode = get_index_node(Bucket, Stream),
    Pid = self(),
    Args = {coord_put_conditionally, N, W, Bucket, Stream, Data, LastSeqNum, Pid},
    ReqID = riak_core_vnode_master:sync_spawn_command(IndexNode, Args, iorio_vnode_master),
    wait_for_reqid(ReqID, Timeout).

get_last(Bucket, Stream) ->
    case get(Bucket, Stream, nil, 1) of
        [Blob] -> {ok, Blob};
        [] -> notfound
    end.

get(Bucket, Stream, From) ->
    get(Bucket, Stream, From, 1).

get(Bucket, Stream, From, Count) ->
    iorio_stats:core_get(),
    IndexNode = get_index_node(Bucket, Stream),
    riak_core_vnode_master:sync_spawn_command(IndexNode,
                                              {get, Bucket, Stream, From, Count},
                                              iorio_vnode_master).

subscribe(Bucket, Stream, Pid) ->
    subscribe(Bucket, Stream, nil, Pid).

subscribe(Bucket, Stream, FromSeqNum, Pid) ->
    iorio_stats:core_subscribe(),
    IndexNode = get_index_node(Bucket, Stream),
    riak_core_vnode_master:sync_spawn_command(IndexNode,
                                              {subscribe, Bucket, Stream, FromSeqNum, Pid},
                                              iorio_vnode_master).

unsubscribe(Bucket, Stream, Pid) ->
    iorio_stats:core_unsubscribe(),
    IndexNode = get_index_node(Bucket, Stream),
    riak_core_vnode_master:sync_spawn_command(IndexNode,
                                              {unsubscribe, Bucket, Stream, Pid},
                                              iorio_vnode_master).

list(_State) ->
    iorio_stats:core_list_buckets(),
    Timeout = 5000,
    iorio_coverage_fsm:start({list_buckets}, Timeout).

list(State, Bucket) ->
    list(State, Bucket, ?DEFAULT_TIMEOUT_MS).

list(_State, Bucket, Timeout) ->
    iorio_stats:core_list_streams(),
    iorio_coverage_fsm:start({list_streams, Bucket}, Timeout).

bucket_size(Bucket) ->
    bucket_size(Bucket, ?DEFAULT_TIMEOUT_MS).

bucket_size(Bucket, Timeout) ->
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

truncate_percentage(Bucket, Percentage) ->
    iorio_stats:core_truncate(),
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

truncate(Bucket, MaxSizeBytes) ->
    {TotalSizeBytes, _} = bucket_size(Bucket),
    if TotalSizeBytes > MaxSizeBytes ->
           Percentage = (MaxSizeBytes * 0.5) / TotalSizeBytes,
           truncate_percentage(Bucket, Percentage);
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

to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(V) when is_integer(V) -> integer_to_list(V).

