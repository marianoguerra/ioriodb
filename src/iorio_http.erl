-module(iorio_http).
-export([response/2, ok/1, no_permission/1, invalid_body/1, json_response/2,
        error/3, unauthorized/1]).

response(Body, Req) ->
    cowboy_req:set_resp_body(Body, Req).

ok(Req) ->
    response(<<"{\"ok\": true}">>, Req).

error(Req, Type, Reason) ->
    json_response(Req, [{type, Type}, {reason, Reason}]).

no_permission(Req) ->
    response(<<"{\"type\": \"no-perm\"}">>, Req).

unauthorized(Req) ->
    response(<<"{\"type\": \"unauthorized\"}">>, Req).

invalid_body(Req) ->
    error(Req, <<"invalid-body">>, <<"Invalid Request Body">>).

json_response(Req, Body) ->
    JsonBody = jsx:encode(Body),
    Header = <<"Content-Type">>,
    ContentType = <<"application/json">>,
    Req1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    Req2 = cowboy_req:set_resp_header(Header, ContentType, Req1),
    response(JsonBody, Req2).
