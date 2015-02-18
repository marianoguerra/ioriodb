-module(iorio_session).
-export([from_request/2,
         fill_session/3,
         fill_session_from_token/3]).

-include_lib("jwt/include/jwt.hrl").
-include("include/iorio.hrl").

get_security_context(Username) ->
    % TODO: don't try catch
    % TODO: this is private
    try
        {ok, riak_core_security:get_context(Username)}
    catch error:badarg ->
        {error, notfound}
    end.

session_from_parsed_body(Body) ->
    Username = proplists:get_value(<<"u">>, Body),
    case get_security_context(Username) of
        {ok, SecurityCtx} -> {ok, {Username, Body, SecurityCtx}};
        Error -> Error
    end.

session_from_parsed_token(BodyRaw) ->
    Body = iorio_json:decode_plist(BodyRaw),
    session_from_parsed_body(Body).

make_anon_session() ->
    session_from_parsed_body([{<<"u">>, <<"anonymous">>}]).

session_from_token(nil, _Secret) ->
    {ok, make_anon_session()};
session_from_token(JWTToken, Secret) ->
    case jwt:decode(JWTToken, Secret) of
        {ok, #jwt{body=BodyRaw}}    -> session_from_parsed_token(BodyRaw);
        {error, {Reason, _Details}} -> {error, Reason};
        {error, Reason}             -> {error, Reason}
    end.

jwt_from_request(Req) ->
    case cowboy_req:header(<<"x-session">>, Req) of
        {undefined, Req1} ->
            cowboy_req:qs_val(<<"jwt">>, Req1, undefined);
        Other -> Other
    end.

from_request(Req, Secret) ->
    case jwt_from_request(Req) of
        {undefined, R1} ->
            {ok, AnonSession} = make_anon_session(),
            {ok, AnonSession, R1};
        {JWTToken, R2}  ->
            case session_from_token(JWTToken, Secret) of
                {ok, Session}   -> {ok, Session, R2};
                {error, Reason} -> {error, Reason, R2}
            end
    end.

fill_session(Req, Access, Info) ->
    Secret = ioriol_access:secret(Access),
    case iorio_session:from_request(Req, Secret) of
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
    case session_from_token(Token, Secret) of
        {ok, {Username, Body, SCtx}} ->
            ioriol_access:update_req(Info, [{username, Username},
                                            {session_body, Body},
                                            {session, SCtx}]);
        Error -> Error
    end.
