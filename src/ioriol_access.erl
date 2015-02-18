-module(ioriol_access).

-export([new/1, new_req/1, update_req/2, handle_req/2,
         is_authorized/2, is_authorized_for_bucket/3, is_authorized_for_stream/3,
         access_details/2, add_group/2, grant/5, authenticate/4]).
-export([secret/1, username/1, session_body/1, bucket/1, stream/1]).
-export([behaviour_info/1]).

-export([maybe_grant_bucket_ownership/2]).

-export_type([access_details/0]).

-ignore_xref([behaviour_info/1]).

-include("include/iorio.hrl").

-record(state, {secret, handler}).
-record(req, {bucket, stream, username, session_body, session, role, action, permission}).

%% XXX should I just pick one?
-type grant() :: binary() | string().
-type grants() :: [grant()].
-type access_detail() :: {users, grants()} | {groups, grants()}.
-type access_details() :: [access_detail()].

-define(IsSet(Val), Val /= undefined).

%% api

new(Opts) ->
    parse_opts(Opts, #state{}).

new_req(Opts) ->
    parse_req_opts(Opts, #req{}).

update_req(Req, Opts) ->
    parse_req_opts(Opts, Req).

is_authorized(State=#state{},
              Req=#req{bucket=Bucket, stream=Stream, username=Username, role=Role,
                       action=Action, permission=Permission})
  when ?IsSet(Bucket), ?IsSet(Stream), ?IsSet(Username), ?IsSet(Role),
       ?IsSet(Action), ?IsSet(Permission)->

    if
        Stream == any ->
            is_authorized_for_bucket(State, Req, ?PERM_BUCKET_GRANT);
        true ->
            is_authorized_for_stream(State, Req, ?PERM_STREAM_GRANT)
    end;

is_authorized(#state{}, Req) ->
    {error, {invalid_req, Req}}.

handle_req(#state{secret=Secret, handler=Handler},
           #req{bucket=Bucket, stream=Stream, username=Username, role=Role,
                action=Action, permission=Permission, session=Session})
  when ?IsSet(Bucket), ?IsSet(Stream), ?IsSet(Username), ?IsSet(Role),
       ?IsSet(Action), ?IsSet(Permission), ?IsSet(Session) ->
    Handler:handle(Username, Secret, Bucket, Stream, Session, Role, Action, Permission);

handle_req(#state{}, Req) ->
    {error, {invalid_req, Req}}.

access_details(#state{handler=Handler, secret=Secret},
               #req{username=Username, session=Session}) ->
    Handler:access_details(Username, Secret, Session).

secret(#state{secret=Val}) -> Val.
username(#req{username=Val}) -> Val.
session_body(#req{session_body=Val}) -> Val.
bucket(#req{bucket=Val}) -> Val.
stream(#req{stream=Val}) -> Val.

maybe_grant_bucket_ownership(State, Username) ->
    HasStream = application:get_env(iorio, user_has_bucket, false),
    maybe_grant_bucket_ownership(State, Username, HasStream).

maybe_grant_bucket_ownership(_State, _Username, false) ->
    ok;
maybe_grant_bucket_ownership(#state{handler=Handler}, Username, true) ->
    Stream = prefix_user_bucket(Username),
    lager:info("granting ~s bucket ownership to ~s", [Stream, Username]),
    Handler:grant_bucket_ownership(Username, Stream).

add_group(#state{handler=Handler}, Name) ->
    Handler:add_group(Name).

grant(#state{handler=Handler}, Role, Bucket, Stream, Permission) ->
    Handler:grant(Role, Bucket, Stream, Permission).

authenticate(#state{handler=Handler}, Req, Username, Password) ->
    case Handler:authenticate(Username, Password) of
        {ok, Fields} -> update_req(Req, Fields);
        Other -> Other
    end.

%% behaviour functions

behaviour_info(callbacks) ->
    [{is_authorized, 4},
     {is_authorized, 5},
     {handle, 8},
     {user_access_details, 3},
     {grant_bucket_ownership, 2},
     {add_group, 1},
     {grant, 4},
     {authenticate, 2},
     {access_details, 4}];

behaviour_info(_Other) ->
    undefined.

%% private functions

parse_opts([], State) ->
    if
        State#state.secret == undefined ->
            {error, secret_not_set};
        State#state.handler == undefined ->
            {error, handler_not_set};
        true ->
            {ok, State}
    end;

parse_opts([{secret, Val}|Opts], State) ->
    parse_opts(Opts, State#state{secret=Val});

parse_opts([{handler, Val}|Opts], State) ->
    parse_opts(Opts, State#state{handler=Val});

parse_opts([Other|Opts], State) ->
    %% XXX crash?
    lager:warning("Unknown option ~p", [Other]),
    parse_opts(Opts, State).

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

parse_req_opts([{role, Val}|Opts], Req) ->
    parse_req_opts(Opts, Req#req{role=Val});

parse_req_opts([{action, Val}|Opts], Req) ->
    parse_req_opts(Opts, Req#req{action=Val});

parse_req_opts([{permission, Val}|Opts], Req) ->
    parse_req_opts(Opts, Req#req{permission=Val});

parse_req_opts([Other|Opts], Req) ->
    %% XXX crash?
    lager:warning("Unknown option ~p", [Other]),
    parse_req_opts(Opts, Req).

update_req_on_is_authorized(Req, {ok, Fields}) ->
    update_req(Req, Fields);
update_req_on_is_authorized(_Req, Other) ->
    Other.

is_authorized_for_bucket(#state{handler=Handler},
                         Req=#req{bucket=Bucket,
                                  username=Username, session=Session},
                         Perm) ->
    Result = Handler:is_authorized(Username, Bucket, Session, Perm),
    update_req_on_is_authorized(Req, Result).

is_authorized_for_stream(#state{handler=Handler},
                         Req=#req{bucket=Bucket, stream=Stream,
                                  username=Username, session=Session},
                         Perm) ->
    Result = Handler:is_authorized(Username, Bucket, Stream, Session, Perm),
    update_req_on_is_authorized(Req, Result).

prefix_user_bucket(Bucket) ->
    Prefix = application:get_env(iorio, user_bucket_prefix, ""),
    list_to_binary(io_lib:format("~s~s", [Prefix, Bucket])).

