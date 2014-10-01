-module(iorio_session).
-export([from_request/2,
         handle_is_authorized/3,
         handle_is_authorized/4,
         handle_is_authorized_for_bucket/6]).

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

unauthorized_response(Reason, Req, State) ->
    lager:info("unauthorized access ~p", [Reason]),
    Req1 = iorio_http:response(<<"{\"type\": \"no-perm\"}">>, Req),
    {{false, <<"jwt">>}, Req1, State}.

handle_is_authorized(Req, Secret, State, SetSession) ->
    case iorio_session:from_request(Req, Secret) of
        {ok, Body, Req11, _Jwt} ->
            Username = proplists:get_value(<<"u">>, Body),
            % TODO: don't try catch
            try
                % TODO: this is private
                SecurityCtx = riak_core_security:get_context(Username),
                StateWithSession = SetSession(State, {Username, Body, SecurityCtx}),
                {true, Req11, StateWithSession}
            catch error:badarg ->
                unauthorized_response(notfound, Req11, State)
            end;
        {error, Reason, Req12, _Jwt} ->
            unauthorized_response(Reason, Req12, State)
    end.

% Bucket is the atom all when operating on all buckets
handle_is_authorized_for_bucket(Req, Secret, State, GetSession, SetSession, Bucket) ->
    Res = handle_is_authorized(Req, Secret, State, SetSession),
    {AuthOk, Req1, State1} = Res,
    Username = case GetSession(State1) of
                   nil -> nil;
                   undefined -> nil;
                   {User, _, _} -> User
               end,


    case {AuthOk, Username, Bucket} of
        % NOTE: for now user can only operate on bucket with his username,
        % except if he is the admin
        {true, <<"admin">>, _} ->
            Res;
        {true, Username, Username} ->
            Res;
        {true, _, _} ->
            Req2 = iorio_http:response(<<"{\"type\": \"no-perm\"}">>, Req1),
            {{false, <<"jwt">>}, Req2, State};
        _ ->
            Res
    end.
