-module(iorio_parse_tests).
-include_lib("eunit/include/eunit.hrl").

parse_valid_test() ->
    ?assertEqual({ok, {<<"foo">>, <<"bar">>}},
                 iorio_parse:subscription(<<"foo:bar">>)),
    ?assertEqual({ok, {<<"foo">>, <<"bar">>, 12}},
                 iorio_parse:subscription(<<"foo:bar:12">>)).

parse_invalid_test() ->
    ?assertEqual({error, badarg}, iorio_parse:subscription(<<"foo">>)),
    ?assertEqual({error, badseqnum}, iorio_parse:subscription(<<"foo:bar:baz">>)),
    ?assertEqual({error, badarg}, iorio_parse:subscription(<<"foo:bar:12:baz">>)).

parse_empty_subs_test() ->
    ?assertEqual([],
                 iorio_parse:subscriptions([])).

parse_valid_subs_test() ->
    ?assertEqual([{<<"foo">>, <<"bar">>}, {<<"a">>, <<"b">>, 0}],
                 iorio_parse:subscriptions([<<"foo:bar">>, <<"a:b:0">>])).

parse_some_valid_subs_test() ->
    ?assertEqual([{<<"foo">>, <<"bar">>}, {<<"a">>, <<"b">>, 0}],
                 iorio_parse:subscriptions([<<"foo:bar">>, <<"a:b:0">>,
                                            <<"a">>, <<"">>])).

parse_no_valid_subs_test() ->
    ?assertEqual([],
                 iorio_parse:subscriptions([<<"foo:bar::">>, <<"a:b:0a">>,
                                            <<"a">>, <<"">>])).
