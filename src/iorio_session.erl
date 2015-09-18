-module(iorio_session).
-export([fill_session/3,
         fill_session_from_token/3]).

-include_lib("jwt/include/jwt.hrl").
-include("include/iorio.hrl").

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

session_from_token(Access, nil, _Secret) ->
    make_anon_session(Access);
session_from_token(Access, JWTToken, Secret) ->
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

from_request(Access, Req, Secret) ->
    case jwt_from_request(Req) of
        {undefined, R1} ->
            {ok, AnonSession} = make_anon_session(Access),
            {ok, AnonSession, R1};
        {JWTToken, R2}  ->
            case session_from_token(Access, JWTToken, Secret) of
                {ok, Session}   -> {ok, Session, R2};
                {error, Reason} -> {error, Reason, R2}
            end
    end.

fill_session(Req, Access, Info) ->
    Secret = ioriol_access:secret(Access),
    case from_request(Access, Req, Secret) of
        {ok, {Username, Body, SCtx}, Req1} ->
            {ok, Info1} = ioriol_access:update_req(Info,
                                                   [{username, Username},
                                                    {session_body, Body},
                                                    {session, SCtx}]),
            {ok, Req1, Info1};
        Error -> Error
    end.

fill_session_from_token(Access, Info, Token) ->
    Secret = ioriol_access:secret(Access),
    case session_from_token(Access, Token, Secret) of
        {ok, {Username, Body, SCtx}} ->
            ioriol_access:update_req(Info, [{username, Username},
                                            {session_body, Body},
                                            {session, SCtx}]);
        Error -> Error
    end.
