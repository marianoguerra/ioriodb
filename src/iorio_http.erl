-module(iorio_http).
-export([response/2, no_permission/1]).

response(Body, Req) ->
    cowboy_req:set_resp_body(Body, Req).

no_permission(Req) ->
    response(<<"{\"type\": \"no-perm\"}">>, Req).

