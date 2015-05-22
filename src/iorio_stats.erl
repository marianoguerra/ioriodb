-module(iorio_stats).
-export([all_stats/0]).

all_stats() ->
 [{node, node_stats()}, {riak_core, riak_core_stats()}].

node_stats() ->
 [{Abs1, Inc1}, {Abs2, Inc2}] = recon:node_stats_list(2, 2000),
 [{abs1, Abs1}, {inc1, Inc1}, {abs2, Abs2}, {inc2, Inc2}].

riak_core_stats() ->
    Stats = riak_core_stat:get_stats(),
    KeyToString = fun ({K, V}) ->
                          StrKeyTokens = lists:map(fun to_string/1, K),
                          StrKey = string:join(StrKeyTokens, "."),
                          BinKey = list_to_binary(StrKey),
                          {BinKey, V}
                  end,
    lists:map(KeyToString, Stats).


to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(V) when is_integer(V) -> integer_to_list(V).
