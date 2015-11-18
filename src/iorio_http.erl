-module(iorio_http).
-export([ok/1, no_permission/1, invalid_body/1, json_response/2,
         error/3, unauthorized/1, set_content_type/2, set_content_type_body/3,
         rate_limited/1, check_rate_limit/5]).

response(Req, Body) ->
    cowboy_req:set_resp_body(Body, Req).

ok(Req) -> json_response(Req, [{ok, true}]).

error(Req, Type, Reason) -> json_response(Req, [{type, Type}, {reason, Reason}]).

no_permission(Req) -> error(Req, <<"no-perm">>, <<"No Permission">>).

unauthorized(Req) -> error(Req, <<"unauthorized">>, <<"Unauthorized">>).

invalid_body(Req) -> error(Req, <<"invalid-body">>, <<"Invalid Request Body">>).

rate_limited(Req) -> error(Req, <<"rate-limited">>, <<"Too Many Requests">>).

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

check_rate_limit(Command, Access, Req, State, SetCreds) ->
    case iorio_session:creds_from_request(Access, Req) of
        {ok, Creds=#{username := Username}, Req1} ->
            State1 = SetCreds(State, Creds),
            case iorio_rlimit:notify(Username, Command) of
                {ok, {_Count, _Time}} ->
                    {false, Req1, State1};
                {over_limit, _Info} ->
                    {true, iorio_http:rate_limited(Req1), State1}
            end;
        {error, Reason, Req1} ->
            lager:error("getting req creds ~p", [Reason]),
            {true, Req1, State}
    end.
