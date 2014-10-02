-module(iorio_session).
-export([from_request/2,
         session_from_token/2,
         is_authorized_for_bucket/2,
         is_authorized_for_stream/3,
         handle_is_authorized/3,
         handle_is_authorized/4,
         handle_is_authorized_for_bucket/6,
         handle_is_authorized_for_stream/7]).

-include_lib("jwt/include/jwt.hrl").

get_security_context(Username) ->
    % TODO: don't try catch
    % TODO: this is private
    try
        {ok, riak_core_security:get_context(Username)}
    catch error:badarg ->
        {error, notfound}
    end.

session_from_parsed_token(BodyRaw) ->
    Body = jsx:decode(BodyRaw),
    Username = proplists:get_value(<<"u">>, Body),
    case get_security_context(Username) of
        {ok, SecurityCtx} -> {ok, {Username, Body, SecurityCtx}};
        Error -> Error
    end.

session_from_token(JWTToken, Secret) ->
    case jwt:decode(JWTToken, Secret) of
        {ok, #jwt{body=BodyRaw}} -> session_from_parsed_token(BodyRaw);
        {error, Reason}          -> {error, Reason};
        {error, Reason, _Jwt2}   -> {error, Reason}
    end.

from_request(Req, Secret) ->
    case cowboy_req:header(<<"x-session">>, Req) of
        {undefined, R1} -> {error, nosession, R1};
        {JWTToken, R2}  ->
            case session_from_token(JWTToken, Secret) of
                {ok, Session}   -> {ok, Session, R2};
                {error, Reason} -> {error, Reason, R2}
            end
    end.

handle_is_authorized(Req, Secret, State) ->
    handle_is_authorized(Req, Secret, State, fun (S, _) -> S end).

unauthorized_response(Reason, Req, State) ->
    lager:info("unauthorized access ~p", [Reason]),
    Req1 = iorio_http:no_permission(Req),
    {{false, <<"jwt">>}, Req1, State}.

handle_is_authorized(Req, Secret, State, SetSession) ->
    case from_request(Req, Secret) of
        {ok, Session, Req1} ->
            State1 = SetSession(State, Session),
            {true, Req1, State1};
        {error, Reason, Req1} ->
            unauthorized_response(Reason, Req1, State)
    end.

% NOTE: for now user can only operate on bucket with his username,
% except if he is the admin
is_authorized_for_bucket(nil, _Bucket)         -> false;
is_authorized_for_bucket(<<"admin">>, _Bucket) -> true;
is_authorized_for_bucket(Username, Username)   -> true;
is_authorized_for_bucket(_Username, _Bucket)   -> false.

% NOTE: we don't still support per stream permissions
is_authorized_for_stream(Username, Bucket, _Stream) ->
    is_authorized_for_bucket(Username, Bucket).

username_from_session(nil)              -> nil;
username_from_session(undefined)        -> nil;
username_from_session({Username, _, _}) -> Username.

% Bucket is the atom all when operating on all buckets
handle_is_authorized_for(Req, Secret, State, GetSession, SetSession, CheckAuth) ->
    Res = handle_is_authorized(Req, Secret, State, SetSession),
    {AuthOk, Req1, State1} = Res,
    Username = username_from_session(GetSession(State1)),
    if AuthOk ->
           IsAuthorized = CheckAuth(Username),
           if IsAuthorized -> Res;
              true ->
                  Req2 = iorio_http:no_permission(Req1),
                  {{false, <<"jwt">>}, Req2, State}
           end;
       true -> Res
    end.

handle_is_authorized_for_bucket(Req, Secret, State, GetSession, SetSession, Bucket) ->
    CheckAuth = fun (Username) -> is_authorized_for_bucket(Username, Bucket) end,
    handle_is_authorized_for(Req, Secret, State, GetSession, SetSession, CheckAuth).

handle_is_authorized_for_stream(Req, Secret, State, GetSession, SetSession, Bucket, Stream) ->
    CheckAuth = fun (Username) -> is_authorized_for_stream(Username, Bucket, Stream) end,
    handle_is_authorized_for(Req, Secret, State, GetSession, SetSession, CheckAuth).
