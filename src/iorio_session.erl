-module(iorio_session).
-export([from_request/2,
         session_from_token/2,
         permission_to_internal/3,
         grant/4,
         revoke/4,
         is_authorized_for_bucket/4,
         is_authorized_for_stream/5,
         handle_is_authorized/3,
         handle_is_authorized/4,
         handle_is_authorized_for_bucket/7,
         handle_is_authorized_for_stream/8]).

-include_lib("jwt/include/jwt.hrl").
-include("include/iorio.hrl").

permission_to_internal(_Bucket, any, <<"get">>) -> ?PERM_BUCKET_GET;
permission_to_internal(_Bucket, any, <<"put">>) -> ?PERM_BUCKET_PUT;
permission_to_internal(_Bucket, any, <<"list">>) -> ?PERM_BUCKET_LIST;
permission_to_internal(_Bucket, any, <<"grant">>) -> ?PERM_BUCKET_GRANT;

permission_to_internal(_Bucket, _Stream, <<"get">>) -> ?PERM_STREAM_GET;
permission_to_internal(_Bucket, _Stream, <<"put">>) -> ?PERM_STREAM_PUT;
permission_to_internal(_Bucket, _Stream, <<"grant">>) -> ?PERM_STREAM_GRANT;
permission_to_internal(_Bucket, _Stream, _Perm) -> notfound.

grant(Username, Bucket, any, Permission) ->
    riak_core_security:add_grant([Username], Bucket, [Permission]);

grant(Username, Bucket, Stream, Permission) ->
    riak_core_security:add_grant([Username], {Bucket, Stream}, [Permission]).

revoke(Username, Bucket, any, Permission) ->
    riak_core_security:add_revoke([Username], Bucket, [Permission]);

revoke(Username, Bucket, Stream, Permission) ->
    riak_core_security:add_revoke([Username], {Bucket, Stream}, [Permission]).

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

% XXX I'm not using the NewCtx returned
can_do_on(Action, Thing, Ctx) ->
    riak_core_security:check_permissions({Action, Thing}, Ctx).

can_do_on_bucket(Ctx, Bucket, Action) ->
    can_do_on(Action, Bucket, Ctx).

can_do_on_stream(Ctx, Bucket, Stream, Action) ->
    can_do_on(Action, {Bucket, Stream}, Ctx).

is_authorized_for_bucket(Ctx, nil, _Bucket, _Action) ->
    {false, "No user", Ctx};
is_authorized_for_bucket(Ctx, <<"admin">>, _Bucket, _Action) -> {true, Ctx};
is_authorized_for_bucket(Ctx, Username, Username, _Action)   -> {true, Ctx};
is_authorized_for_bucket(Ctx, _Username, Bucket, Action)   ->
    can_do_on_bucket(Ctx, Bucket, Action).

is_authorized_for_stream(Ctx, nil, _Bucket, _Stream, _Action) ->
    {false, "No user", Ctx};
is_authorized_for_stream(Ctx, <<"admin">>, _Bucket, _Stream, _Action) -> {true, Ctx};
is_authorized_for_stream(Ctx, Username, Username, _Stream, _Action)   -> {true, Ctx};
is_authorized_for_stream(Ctx, Username, Bucket, Stream, Action) ->
    case is_authorized_for_bucket(Ctx, Username, Bucket, Action) of
        {true, _NewCtx}=Result -> Result;
        _Other -> can_do_on_stream(Ctx, Bucket, Stream, Action)
    end.

% Bucket is the atom any when operating on all buckets
handle_is_authorized_for(Req, Secret, State, GetSession, SetSession, CheckAuth) ->
    Res = handle_is_authorized(Req, Secret, State, SetSession),
    {AuthOk, Req1, State1} = Res,
    Session = GetSession(State1),
    {Username, SessionBody, Ctx} = Session,
    if AuthOk ->
           case CheckAuth(Ctx, Username) of
               {true, NewCtx} ->
                   NewSession = {Username, SessionBody, NewCtx},
                   State2 = SetSession(State1, NewSession),
                   {true, Req1, State2};
               {false, Reason, NewCtx} ->
                  lager:info("unauthorized ~p", [Reason]),
                  NewSession = {Username, SessionBody, NewCtx},
                  State2 = SetSession(State1, NewSession),
                  Req2 = iorio_http:no_permission(Req1),
                  {{false, <<"jwt">>}, Req2, State2}
           end;
       true -> Res
    end.

handle_is_authorized_for_bucket(Req, Secret, State, GetSession, SetSession, Bucket, Action) ->
    CheckAuth = fun (Ctx, Username) ->
                        is_authorized_for_bucket(Ctx, Username, Bucket, Action)
                end,
    handle_is_authorized_for(Req, Secret, State, GetSession, SetSession, CheckAuth).

handle_is_authorized_for_stream(Req, Secret, State, GetSession, SetSession,
                                Bucket, Stream, Action) ->
    CheckAuth = fun (Ctx, Username) ->
                        is_authorized_for_stream(Ctx, Username, Bucket, Stream, Action)
                end,
    handle_is_authorized_for(Req, Secret, State, GetSession, SetSession, CheckAuth).
