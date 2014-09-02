-module(listener_tests).
-include_lib("eunit/include/eunit.hrl").
-include("priv/include/listener.hrl").

rget() -> rget(<<"/">>, <<"">>).
rget(Path, Query) -> req(<<"GET">>,  Path, Query, <<>>).
%rdel(Path, Query) -> req(<<"DELETE">>,  Path, Query, <<>>).
%rpost(Path, Query, Buffer) -> req(<<"POST">>,  Path, Query, Buffer).
%rput(Path, Query, Buffer) -> req(<<"PUT">>,  Path, Query, Buffer).

req(Method, Path, Query, Buffer) ->
    req(Method, Path, Query, Buffer, [], <<"localhost">>, 8080).

%req(Method, Path, Query, Buffer, Headers) ->
%    req(Method, Path, Query, Buffer, Headers, <<"localhost">>, 8080).

req(Method, Path, Query, Buffer, Headers, Host, Port) ->
    cowboy_req:new(nil, nil, nil, Method, Path, Query, 'HTTP/1.1', Headers, 
                   Host, Port, Buffer, false, false, nil).

init_test_() ->
    Req = rget(),
    {ok, Req, State} = iorio_listen_handler:init(nil, Req, [], false),
    [?_assertEqual(State#state.channels, []),
     ?_assertEqual(State#state.iorio, iorio)].

init_override_iorio_test_() ->
    Req = rget(),
    {ok, Req, State} = iorio_listen_handler:init(nil, Req, [{iorio, dummy_iorio}], false),
    [?_assertEqual(State#state.channels, []),
     ?_assertEqual(State#state.iorio, dummy_iorio)].

command_to_json(Id, Command, Rest) ->
    jsx:encode([{cmd, Command}, {id, Id}] ++ Rest).

subscribe_to_json(Id, Bucket, Stream) ->
    command_to_json(Id, <<"subscribe">>, [{bucket, Bucket}, {stream, Stream}]).

unsubscribe_to_json(Id, Bucket, Stream) ->
    command_to_json(Id, <<"unsubscribe">>, [{bucket, Bucket}, {stream, Stream}]).

handle_ping_test_() ->
    Body = command_to_json(1, <<"ping">>, []),
    {Ok, Id, Reason, _State1} = get_resp_field(Body),
    [?_assertEqual(true, Ok),
     ?_assertEqual(undefined, Reason),
     ?_assertEqual(1, Id)].

get_resp_field(Body) ->
    Req = rget(),
    {ok, Req, State} = iorio_listen_handler:init(nil, Req, [{iorio, dummy_iorio}], false),
    {reply, RespBin, _Req1, State1} = iorio_listen_handler:stream(Body, Req, State),
    Resp = jsx:decode(RespBin),
    Ok = proplists:get_value(<<"ok">>, Resp, undefined),
    Id = proplists:get_value(<<"id">>, Resp, undefined),
    Reason = proplists:get_value(<<"reason">>, Resp, undefined),
    {Ok, Id, Reason, State1}.

handle_unknown_command_test_() ->
    Body = command_to_json(1, <<"asd">>, []),
    {Ok, Id, Reason, _State1} = get_resp_field(Body),
    [?_assertEqual(false, Ok),
     ?_assertEqual(<<"unknown command">>, Reason),
     ?_assertEqual(1, Id)].

handle_subscribe_missing_bucket_test_() ->
    Body = command_to_json(1, <<"subscribe">>, [{stream, <<"stream1">>}]),
    {Ok, Id, Reason, _State1} = get_resp_field(Body),
    [?_assertEqual(false, Ok),
     ?_assertEqual(<<"missing bucket">>, Reason),
     ?_assertEqual(1, Id)].

handle_subscribe_missing_stream_test_() ->
    Body = command_to_json(1, <<"subscribe">>, [{bucket, <<"bucket1">>}]),
    {Ok, Id, Reason, _State1} = get_resp_field(Body),
    [?_assertEqual(false, Ok),
     ?_assertEqual(<<"missing stream">>, Reason),
     ?_assertEqual(1, Id)].

handle_subscribe_missing_stream_and_buckets_test_() ->
    Body = command_to_json(1, <<"subscribe">>, []),
    {Ok, Id, Reason, _State1} = get_resp_field(Body),
    [?_assertEqual(false, Ok),
     ?_assertEqual(<<"missing bucket and stream">>, Reason),
     ?_assertEqual(1, Id)].

handle_subscribe_test_() ->
    Body = subscribe_to_json(1, <<"bucket1">>, <<"stream1">>),
    {Ok, Id, Reason, State1} = get_resp_field(Body),
    [?_assertEqual([{<<"bucket1">>, <<"stream1">>}], State1#state.channels),
     ?_assertEqual(undefined, Reason),
     ?_assertEqual(true, Ok),
     ?_assertEqual(1, Id)].

handle_subscribe_unsubscribe_test_() ->
    Body1 = subscribe_to_json(1, <<"bucket1">>, <<"stream1">>),
    Body2 = unsubscribe_to_json(2, <<"bucket1">>, <<"stream1">>),

    Req = rget(),
    {ok, Req, State} = iorio_listen_handler:init(nil, Req, [{iorio, dummy_iorio}], false),
    {reply, RespBin1, _Req1, State1} = iorio_listen_handler:stream(Body1, Req, State),
    {reply, RespBin2, _Req2, State2} = iorio_listen_handler:stream(Body2, Req, State1),

    Resp1 = jsx:decode(RespBin1),
    Ok1 = proplists:get_value(<<"ok">>, Resp1, undefined),
    Id1 = proplists:get_value(<<"id">>, Resp1, undefined),

    Resp2 = jsx:decode(RespBin2),
    Ok2 = proplists:get_value(<<"ok">>, Resp2, undefined),
    Id2 = proplists:get_value(<<"id">>, Resp2, undefined),

    [?_assertEqual([{<<"bucket1">>, <<"stream1">>}], State1#state.channels),
     ?_assertEqual(true, Ok1),
     ?_assertEqual(1, Id1),

     ?_assertEqual([], State2#state.channels),
     ?_assertEqual(true, Ok2),
     ?_assertEqual(2, Id2)].

handle_subscribe_twice_test_() ->
    Body1 = subscribe_to_json(1, <<"bucket1">>, <<"stream1">>),
    Body2 = subscribe_to_json(2, <<"bucket1">>, <<"stream1">>),

    Req = rget(),
    {ok, Req, State} = iorio_listen_handler:init(nil, Req, [{iorio, dummy_iorio}], false),
    {reply, RespBin1, _Req1, State1} = iorio_listen_handler:stream(Body1, Req, State),
    {reply, RespBin2, _Req2, State2} = iorio_listen_handler:stream(Body2, Req, State1),

    Resp1 = jsx:decode(RespBin1),
    Ok1 = proplists:get_value(<<"ok">>, Resp1, undefined),
    Id1 = proplists:get_value(<<"id">>, Resp1, undefined),
    Reason1 = proplists:get_value(<<"reason">>, Resp1, undefined),

    Resp2 = jsx:decode(RespBin2),
    Ok2 = proplists:get_value(<<"ok">>, Resp2, undefined),
    Id2 = proplists:get_value(<<"id">>, Resp2, undefined),
    Reason2 = proplists:get_value(<<"reason">>, Resp2, undefined),

    [?_assertEqual([{<<"bucket1">>, <<"stream1">>}], State1#state.channels),
     ?_assertEqual(true, Ok1),
     ?_assertEqual(1, Id1),
     ?_assertEqual(undefined, Reason1),

     ?_assertEqual([{<<"bucket1">>, <<"stream1">>}], State2#state.channels),
     ?_assertEqual(false, Ok2),
     ?_assertEqual(2, Id2),
     ?_assertEqual(<<"already subscribed">>, Reason2)].

handle_unsubscribe_not_subscribed_test_() ->
    Body = unsubscribe_to_json(1, <<"bucket1">>, <<"stream1">>),
    {Ok, Id, Reason, State1} = get_resp_field(Body),

    [?_assertEqual([], State1#state.channels),
     ?_assertEqual(false, Ok),
     ?_assertEqual(1, Id),
     ?_assertEqual(<<"not subscribed">>, Reason)].

