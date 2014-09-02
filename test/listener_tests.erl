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
    {ok, Req, State} = iorio_listen_handler:init(nil, Req, [{iorio, asd}], false),
    [?_assertEqual(State#state.channels, []),
     ?_assertEqual(State#state.iorio, asd)].

handle_ping_test_() ->
    {reply, Resp, req, state} = iorio_listen_handler:stream(
                                  <<"{\"cmd\":\"ping\",\"id\":12}">>, req, state),
    [?_assertEqual(Resp, iorio_listen_handler:encode_ok(12))].


