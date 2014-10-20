-module(iorio_http).
-export([response/2, ok/1, no_permission/1, invalid_body/1, json_response/2,
        error/3]).

response(Body, Req) ->
    cowboy_req:set_resp_body(Body, Req).

ok(Req) ->
    response(<<"{\"ok\": true}">>, Req).

error(Req, Type, Reason) ->
    json_response(Req, [{type, Type}, {reason, Reason}]).

no_permission(Req) ->
    response(<<"{\"type\": \"no-perm\"}">>, Req).

invalid_body(Req) ->
    response(<<"{\"type\": \"invalid-body\"}">>, Req).

json_response(Req, Body) ->
    JsonBody = jsx:encode(Body),
    response(JsonBody, Req).
