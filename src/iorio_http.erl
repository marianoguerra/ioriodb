-module(iorio_http).
-export([ok/1, no_permission/1, invalid_body/1, json_response/2,
        error/3, unauthorized/1, set_content_type/2, set_content_type_body/3]).

response(Req, Body) ->
    cowboy_req:set_resp_body(Body, Req).

ok(Req) -> json_response(Req, [{ok, true}]).

error(Req, Type, Reason) -> json_response(Req, [{type, Type}, {reason, Reason}]).

no_permission(Req) -> error(Req, <<"no-perm">>, <<"No Permission">>).

unauthorized(Req) -> error(Req, <<"unauthorized">>, <<"Unauthorized">>).

invalid_body(Req) -> error(Req, <<"invalid-body">>, <<"Invalid Request Body">>).

json_response(Req, Body) ->
    JsonBody = iorio_json:encode(Body),
    ContentType = <<"application/json">>,
    set_content_type_body(Req, ContentType, JsonBody).

set_content_type(Req, ContentType) ->
    Header = <<"Content-Type">>,
    Req1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    cowboy_req:set_resp_header(Header, ContentType, Req1).

set_content_type_body(Req, ContentType, Body) ->
    Req1 = set_content_type(Req, ContentType),
    response(Req1, Body).
