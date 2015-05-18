-module(iorio_stats).
-export([all_stats/0]).

all_stats() ->
 [{node, node_stats()}].

node_stats() ->
 [{Abs1, Inc1}, {Abs2, Inc2}] = recon:node_stats_list(2, 2000),
 [{abs1, Abs1}, {inc1, Inc1}, {abs2, Abs2}, {inc2, Inc2}].
