-module(ioriol_access).

%% NOTE: this module assumes the auth_mod is stateful and this module doesn't
%% require to update auth_state when is returned

-export([new/1, new_req/1, update_req/2, is_authorized_for_grant/2,
         is_authorized_for_bucket/3, is_authorized_for_stream/3,
         is_authorized_for_bucket/4, is_authorized_for_stream/5,
         access_details/2, add_group/2, grant/5, revoke/5,
         authenticate/3, authenticate/4]).

-export([secret/1, username/1, session_body/1, bucket/1, stream/1]).

-export([maybe_grant_bucket_ownership/2, maybe_grant_bucket_ownership/3,
         permission_to_internal/3]).

-export([create_user/3, create_user/4, update_user_password/3, users/1,
         get_session/2]).

-export_type([access_details/0]).

-ignore_xref([create_user/3, create_user/4, update_user_password/3]).

-include("include/iorio.hrl").
-include_lib("permiso/include/permiso.hrl").

-record(state, {secret, auth_mod, auth_state}).
-record(req, {bucket, stream, username, session_body, session}).

%% XXX should I just pick one?
-type grant() :: binary() | string().
-type grants() :: [grant()].
-type access_detail() :: {users, grants()} | {groups, grants()}.
-type access_details() :: [access_detail()].

-define(IsSet(Val), Val /= undefined).

%% api

new(Opts) ->
    {secret, Secret} = proplists:lookup(secret, Opts),
    {auth_mod, AuthMod} = proplists:lookup(auth_mod, Opts),
    AuthModOpts = proplists:get_value(auth_mod_opts, Opts, []),
    {ok, AuthState} = AuthMod:new(AuthModOpts),
    State = #state{secret=Secret, auth_mod=AuthMod, auth_state=AuthState},
    {ok, State}.

new_req(Opts) ->
    parse_req_opts(Opts, #req{}).

update_req(Req, Opts) ->
    parse_req_opts(Opts, Req).

is_authorized_for_grant(State=#state{},
                        Req=#req{bucket=Bucket, stream=Stream, username=Username})
  when ?IsSet(Bucket), ?IsSet(Stream), ?IsSet(Username) ->

    if Stream == any ->
           is_authorized_for_bucket(State, Req, ?PERM_BUCKET_GRANT);
       true ->
           is_authorized_for_stream(State, Req, ?PERM_STREAM_GRANT)
    end;

is_authorized_for_grant(#state{}, Req) ->
    {error, {invalid_req, Req}}.

is_authorized_for_bucket(State, Req=#req{bucket=Bucket, session=Session},
                         Perm) ->
    Result = is_authorized_for_bucket(State, Session, Bucket, Perm),
    wrap_ok_with_req(Req, Result).

is_authorized_for_stream(State, Req=#req{bucket=Bucket, stream=Stream,
                                         session=Session}, Perm) ->
    Result = is_authorized_for_stream(State, Session, Bucket, Stream, Perm),
    wrap_ok_with_req(Req, Result).

is_authorized_for_bucket(#state{auth_mod=AuthMod, auth_state=AuthState},
              Session, Bucket, Perm) ->
    user_allowed(AuthMod, AuthState, Session , Bucket, Perm).

is_authorized_for_stream(#state{auth_mod=AuthMod, auth_state=AuthState},
                         Session, Bucket, Stream, Perm) ->
    user_allowed(AuthMod, AuthState, Session , {Bucket, Stream}, Perm).

access_details(#state{auth_mod=AuthMod, auth_state=AuthState},
               #req{bucket=Bucket, stream=Stream}) ->
    case AuthMod:resource_get(AuthState, {Bucket, Stream}) of
        {ok, Resource} -> {ok, resource_perms_to_public(Resource)};
        Other -> Other
    end.

secret(#state{secret=Val}) -> Val.
username(#req{username=Val}) -> Val.
session_body(#req{session_body=Val}) -> Val.
bucket(#req{bucket=Val}) -> Val.
stream(#req{stream=Val}) -> Val.

maybe_grant_bucket_ownership(#state{auth_mod=AuthMod, auth_state=AuthState},
                             Username) ->
    maybe_grant_bucket_ownership(AuthMod, AuthState, Username).

maybe_grant_bucket_ownership(AuthMod, AuthState, Username) ->
    HasStream = application:get_env(iorio, user_has_bucket, false),
    if HasStream ->
        Bucket = prefix_user_bucket(Username),
        lager:info("granting ~s bucket ownership to ~s", [Bucket, Username]),
        Permissions = [?PERM_BUCKET_GET, ?PERM_BUCKET_PUT, ?PERM_BUCKET_GRANT,
                       ?PERM_BUCKET_LIST],
        Grant = #grant{resource={Bucket, any}, permissions=Permissions},
        drop_ok_state(AuthMod:user_grant(AuthState, Username, Grant));
       true -> ok
    end.

add_group(#state{auth_mod=AuthMod, auth_state=AuthState}, Name) ->
    Group = #group{name=Name},
    drop_ok_state(AuthMod:group_add(AuthState, Group)).

grant(#state{auth_mod=AuthMod, auth_state=AuthState}, Username, Bucket, Stream,
      Permission) ->
    lager:info("grant ~p ~p/~p: ~p", [Username, Bucket, Stream, Permission]),
    Grant = #grant{resource={Bucket, Stream}, permissions=[Permission]},
    drop_ok_state(AuthMod:user_grant(AuthState, Username, Grant)).

revoke(#state{auth_mod=AuthMod, auth_state=AuthState}, Username, Bucket, Stream,
      Permission) ->
    lager:info("revoke ~p ~p/~p: ~p", [Username, Bucket, Stream, Permission]),
    Grant = #grant{resource={Bucket, Stream}, permissions=[Permission]},
    drop_ok_state(AuthMod:user_revoke(AuthState, Username, Grant)).

authenticate(State, Req, Username, Password) ->
    case authenticate(State, Username, Password) of
        {ok, Session} ->
            Body = [{u, Username}],
            Fields = [{username, Username}, {session_body, Body}, {session, Session}],
            update_req(Req, Fields);
        Other -> Other
    end.

authenticate(#state{auth_mod=AuthMod, auth_state=AuthState}, Username, Password) ->
    AuthMod:user_auth(AuthState, Username, Password).

create_user(State, Username, Password) ->
    create_user(State, Username, Password, ?DEFAULT_USER_GROUPS).

create_user(#state{auth_mod=AuthMod, auth_state=AuthState}, Username, Password,
            Groups) ->
    User = #user{username=Username, password=Password, groups=Groups},
    drop_ok_state(AuthMod:user_add(AuthState, User)).

update_user_password(#state{auth_mod=AuthMod, auth_state=AuthState}, Username, Password) ->
    drop_ok_state(AuthMod:user_passwd(AuthState, Username, Password)).

users(#state{auth_mod=AuthMod, auth_state=AuthState}) ->
    AuthMod:user_list(AuthState).

get_session(#state{auth_mod=AuthMod, auth_state=AuthState}, Username) ->
    AuthMod:user_context(AuthState, Username).

%% private functions

%% a request can start incomplete
parse_req_opts([], Req) ->
    {ok, Req};

parse_req_opts([{bucket, Val}|Opts], Req) ->
    parse_req_opts(Opts, Req#req{bucket=Val});

parse_req_opts([{stream, Val}|Opts], Req) ->
    parse_req_opts(Opts, Req#req{stream=Val});

parse_req_opts([{username, Val}|Opts], Req) ->
    parse_req_opts(Opts, Req#req{username=Val});

parse_req_opts([{session_body, Val}|Opts], Req) ->
    parse_req_opts(Opts, Req#req{session_body=Val});

parse_req_opts([{session, Val}|Opts], Req) ->
    parse_req_opts(Opts, Req#req{session=Val});

parse_req_opts([Other|Opts], Req) ->
    %% XXX crash?
    lager:warning("Unknown option ~p", [Other]),
    parse_req_opts(Opts, Req).

prefix_user_bucket(Bucket) ->
    Prefix = application:get_env(iorio, user_bucket_prefix, ""),
    list_to_binary(io_lib:format("~s~s", [Prefix, Bucket])).

internal_to_permission({_Bucket, any}, ?PERM_BUCKET_GET) -> <<"get">>;
internal_to_permission({_Bucket, any}, ?PERM_BUCKET_PUT) -> <<"put">>;
internal_to_permission({_Bucket, any}, ?PERM_BUCKET_LIST) -> <<"list">>;
internal_to_permission({_Bucket, any}, ?PERM_BUCKET_GRANT) -> <<"grant">>;

internal_to_permission({_Bucket, _Stream}, ?PERM_STREAM_GET) -> <<"get">>;
internal_to_permission({_Bucket, _Stream}, ?PERM_STREAM_PUT) -> <<"put">>;
internal_to_permission({_Bucket, _Stream}, ?PERM_STREAM_GRANT) -> <<"grant">>;

internal_to_permission({_Bucket, _Stream}, ?PERM_ADMIN_USERS) -> <<"adminusers">>.

internals_to_permissions(Id, Perms, Name) ->
    {Name, lists:map(fun (Perm) ->
                      internal_to_permission(Id, Perm)
              end, Perms)}.

resource_perms_to_public(R=#resource{id=Id, user_grants=UPerms, group_grants=GPerms}) ->
    ToPublic = fun ({Name, Perms}) ->
                       internals_to_permissions(Id, Perms, Name)
               end,
    NewUPerms = lists:map(ToPublic, UPerms),
    NewGPerms = lists:map(ToPublic, GPerms),
    R#resource{user_grants=NewUPerms, group_grants=NewGPerms}.

drop_ok_state({ok, _State}) -> ok;
drop_ok_state(Other) -> Other.

user_allowed(AuthMod, AuthState, Session, Resource, Perm) ->
    IsAuthorized = AuthMod:user_allowed(AuthState, Session, Resource, [Perm]),
    case IsAuthorized of
        true -> ok;
        false -> {error, unauthorized}
    end.

permission_to_internal(_Bucket, any, <<"get">>) -> ?PERM_BUCKET_GET;
permission_to_internal(_Bucket, any, <<"put">>) -> ?PERM_BUCKET_PUT;
permission_to_internal(_Bucket, any, <<"list">>) -> ?PERM_BUCKET_LIST;
permission_to_internal(_Bucket, any, <<"grant">>) -> ?PERM_BUCKET_GRANT;

permission_to_internal(_Bucket, _Stream, <<"get">>) -> ?PERM_STREAM_GET;
permission_to_internal(_Bucket, _Stream, <<"put">>) -> ?PERM_STREAM_PUT;
permission_to_internal(_Bucket, _Stream, <<"grant">>) -> ?PERM_STREAM_GRANT;

permission_to_internal(_Bucket, _Stream, <<"adminusers">>) -> ?PERM_ADMIN_USERS.

wrap_ok_with_req(Req, ok) -> {ok, Req};
wrap_ok_with_req(_Req, Other) -> Other.
