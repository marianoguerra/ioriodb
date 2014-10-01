-module(iorio_http).
-export([response/2]).

response(Body, Req) ->
    cowboy_req:set_resp_body(Body, Req).

