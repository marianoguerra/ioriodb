-module(iorio_session).
-export([from_request/2, handle_is_authorized/3, handle_is_authorized/4]).

-include_lib("jwt/include/jwt.hrl").

from_request(Req, Secret) ->
    case cowboy_req:header(<<"x-session">>, Req) of
        {undefined, R11} -> {error, nosession, R11, nil};
        {JWTToken, R12} ->
            case jwt:decode(JWTToken, Secret) of
                {ok, Jwt1=#jwt{body=BodyRaw}} ->
                    Body = jsx:decode(BodyRaw),
                    {ok, Body, R12, Jwt1};
                {error, Reason, Jwt2} ->
                    {error, Reason, R12, Jwt2}
            end
    end.

handle_is_authorized(Req, Secret, State) ->
    handle_is_authorized(Req, Secret, State, fun (S, _) -> S end).

handle_is_authorized(Req, Secret, State, SetSession) ->
    case iorio_session:from_request(Req, Secret) of
        {ok, Body, Req11, _Jwt} ->
            Username = proplists:get_value(<<"u">>, Body),
            SecurityCtx = riak_core_security:get_context(Username),
            StateWithSession = SetSession(State, {Username, Body, SecurityCtx}),
            {true, Req11, StateWithSession};

        {error, Reason, Req12, _Jwt} ->
            lager:info("unauthorized access ~p", [Reason]),
            {{false, <<"jwt">>}, Req12, State}
    end.
