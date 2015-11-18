-module(iorio_session).
-export([fill_session/3,
         fill_session_from_token/3,
         from_request/2,
         creds_from_request/2,
         new_session_body/1,
         auth_ok_response/5]).

-include_lib("jwt/include/jwt.hrl").
-include("include/iorio.hrl").

new_session_body(Username) -> [{u, Username}].

auth_ok_response(Req, Username, Algorithm, Secret, SessionDurationSecs) ->
    Expiration = jwt:now_secs() + SessionDurationSecs,
    SessionBody = new_session_body(Username),
    {ok, Token} = jwt:encode(Algorithm, SessionBody, Secret,
                             [{exp, Expiration}]),
    ResultJson = [{ok, true}, {token, Token}, {username, Username}],
    ResultJsonBin = iorio_json:encode(ResultJson),
    cowboy_req:set_resp_body(ResultJsonBin, Req).

session_from_parsed_body(Access, Body) ->
    Username = proplists:get_value(<<"u">>, Body),
    case ioriol_access:get_session(Access, Username) of
        {ok, SecurityCtx} -> {ok, {Username, Body, SecurityCtx}};
        Error -> Error
    end.

session_from_parsed_token(Access, BodyRaw) ->
    Body = iorio_json:decode_plist(BodyRaw),
    session_from_parsed_body(Access, Body).

make_anon_session(Access) ->
    session_from_parsed_body(Access, [{<<"u">>, <<"anonymous">>}]).

session_from_token(Access, nil) ->
    make_anon_session(Access);
session_from_token(Access, JWTToken) ->
    Secret = ioriol_access:secret(Access),
    case jwt:decode(JWTToken, Secret) of
        {ok, #jwt{body=BodyRaw}}    -> session_from_parsed_token(Access, BodyRaw);
        {error, {Reason, _Details}} -> {error, Reason};
        {error, Reason}             -> {error, Reason}
    end.

jwt_from_request(Req) ->
    case cowboy_req:header(<<"x-session">>, Req) of
        {undefined, Req1} ->
            cowboy_req:qs_val(<<"jwt">>, Req1, undefined);
        Other -> Other
    end.

from_request(Access, Req) ->
    case jwt_from_request(Req) of
        {undefined, R1} ->
            {ok, AnonSession} = make_anon_session(Access),
            {ok, AnonSession, R1};
        {JWTToken, R2}  ->
            case session_from_token(Access, JWTToken) of
                {ok, Session}   -> {ok, Session, R2};
                {error, Reason} -> {error, Reason, R2}
            end
    end.

fill_session(Req, Access, Info) ->
    case from_request(Access, Req) of
        {ok, {Username, Body, SCtx}, Req1} ->
            {ok, Info1} = ioriol_access:update_req(Info,
                                                   [{username, Username},
                                                    {session_body, Body},
                                                    {session, SCtx}]),
            {ok, Req1, Info1};
        Error -> Error
    end.

fill_session_from_token(Access, Info, Token) ->
    case session_from_token(Access, Token) of
        {ok, {Username, Body, SCtx}} ->
            ioriol_access:update_req(Info, [{username, Username},
                                            {session_body, Body},
                                            {session, SCtx}]);
        Error -> Error
    end.

creds_from_request(Access, Req) ->
    case from_request(Access, Req) of
        {ok, {Username, Body, Ctx}, Req1} ->
            {ok, #{username => Username, jwt_body => Body, ctx => Ctx}, Req1};
        {error, _Reason, _Req1}=Error -> Error
    end.
