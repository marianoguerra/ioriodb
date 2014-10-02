-module(iorio_cbuf_tests).
-include_lib("eunit/include/eunit.hrl").

new() -> iorio_cbuf:new(4).

from_list(Items) ->
    from_list(Items, 10).

from_list(Items, MinSize) ->
    B = iorio_cbuf:new(MinSize),
    lists:foldl(fun (I, Buf) -> iorio_cbuf:add(Buf, I) end, B, Items).

takewhile_reverse_on_empty_test() ->
    B = new(),
    ?assertEqual([], iorio_cbuf:takewhile_reverse(B, fun (_) -> true end)).

takewhile_reverse_test() ->
    B = from_list([1, 2, 3, 4, 5, 6]),

    ?assertEqual([6, 5, 4],
                 iorio_cbuf:takewhile_reverse(B, fun (I) -> I  > 3 end)).

takewhile_reverse_over_min_size_test() ->
    B = from_list([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], 2),

    ?assertEqual([10, 9, 8, 7],
                 iorio_cbuf:takewhile_reverse(B, fun (I) -> I  > 3 end)).
