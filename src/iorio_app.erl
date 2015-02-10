-module(iorio_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("include/iorio.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % TODO: see where to start it
    file_handle_cache:start_link(),
    % TODO: check here that secret is binary and algorigthm is a valid one
    {ok, ApiSecret} = application:get_env(iorio, secret),
    {ok, ApiAlgorithm} = application:get_env(iorio, algorithm),

    AdminUsername = application:get_env(iorio, admin_username, "admin"),
    {ok, AdminPassword} = application:get_env(iorio, admin_password),

    AnonUsername = application:get_env(iorio, anon_username, "anonymous"),
    % default password since login in as anonymous is not that useful
    AnonPassword = application:get_env(iorio, anon_password, <<"secret">>),

    SessionDurationSecs = application:get_env(iorio, session_duration_secs, 3600),

    N = application:get_env(iorio, req_n, 3),
    W = application:get_env(iorio, req_w, 3),
    Timeout = application:get_env(iorio, req_timeout, 5000),

    create_groups(),

    GrantAdminUsers = fun (Username) ->
                              Res = iorio_session:grant(AdminUsername,
                                                        ?PERM_MAGIC_BUCKET, any,
                                                        ?PERM_ADMIN_USERS),
                              lager:info("assign admin users to ~p: ~p",
                                         [Username, Res]),
                              iorio_session:maybe_grant_bucket_ownership(AdminUsername)
                      end,
    create_user(AdminUsername, AdminPassword, ?DEFAULT_ADMIN_GROUPS, GrantAdminUsers),
    create_user(AnonUsername, AnonPassword, ?DEFAULT_ANONYMOUS_GROUPS, fun (_) -> ok end),

    setup_initial_permissions(AdminUsername),

    BaseDispatchRoutes = [
               {"/listen", bullet_handler, [{handler, iorio_listen_handler}, {secret, ApiSecret}]},
               {"/streams/:bucket", iorio_list_handler, [{secret, ApiSecret}]},
               {"/streams/:bucket/:stream", iorio_stream_handler,
                [{secret, ApiSecret}, {n, N}, {w, W}, {timeout, Timeout}]},
               {"/buckets/", iorio_list_handler, [{secret, ApiSecret}]},
               {"/access/:bucket/", iorio_access_handler, [{secret, ApiSecret}]},
               {"/access/:bucket/:stream", iorio_access_handler, [{secret, ApiSecret}]},

               {"/sessions", iorio_session_handler,
                [{secret, ApiSecret}, {algorithm, ApiAlgorithm},
                 {session_duration_secs, SessionDurationSecs}]},
               {"/users/", iorio_user_handler, [{secret, ApiSecret}]},
               {"/ping", iorio_ping_handler, []}
    ],

    UserDispatchRoutes = application:get_env(iorio, api_handlers, []),
    lager:info("configuring routes with following user provided routes: ~p",
               [UserDispatchRoutes]),
    DispatchRoutes = BaseDispatchRoutes ++ UserDispatchRoutes,

    Dispatch = cowboy_router:compile([{'_', DispatchRoutes}]),

    ApiPort = application:get_env(iorio, port, 8080),
    ApiAcceptors = application:get_env(iorio, nb_acceptors, 100),
    {ok, _} = cowboy:start_http(http, ApiAcceptors, [{port, ApiPort}],
                                [{env, [{dispatch, Dispatch}]}]),

    SecureEnabled = application:get_env(iorio, secure_enabled, false),
    SecureApiPort = application:get_env(iorio, secure_port, 8443),

    if
        SecureEnabled ->
            lager:info("secure api enabled, starting"),
            SSLCACertPath = application:get_env(iorio, secure_cacert, notset),
            {ok, SSLCertPath} = application:get_env(iorio, secure_cert),
            {ok, SSLKeyPath} = application:get_env(iorio, secure_key),

            BaseSSLOpts = [{port, SecureApiPort}, {certfile, SSLCertPath},
                           {keyfile, SSLKeyPath}],

            SSLOpts = if SSLCACertPath == notset -> BaseSSLOpts;
                         true -> [{cacertfile, SSLCACertPath}|BaseSSLOpts]
                      end,

            {ok, _} = cowboy:start_https(https, ApiAcceptors, SSLOpts,
                                         [{env, [{dispatch, Dispatch}]}]);
        true ->
            lager:info("secure api disabled"),
            ok
    end,

    case iorio_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, iorio_vnode}]),

            ok = riak_core_ring_events:add_guarded_handler(iorio_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(iorio_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(iorio, self()),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.

%% private api

create_user(Username, Password, Groups, OnUserCreated) ->
    case iorio_user:create(Username, Password, Groups) of
        ok ->
            lager:info("~p user created", [Username]),
            OnUserCreated(Username);
        {error, role_exists}  ->
            lager:info("~p user exists", [Username]);
        OtherError ->
            lager:error("creating ~p user ~p", [Username, OtherError])
    end.

setup_initial_permissions(AdminUsername) ->
    PublicReadBucket = <<"public">>,
    R1 = iorio_session:grant(<<"*">>, PublicReadBucket, any, "iorio.get"),
    lager:info("set read permissions to ~s to all: ~p",
               [PublicReadBucket, R1]),
    R2 = iorio_session:grant(list_to_binary(AdminUsername), PublicReadBucket,
                             any, "iorio.put"),
    lager:info("set write permissions to ~s to ~p: ~p",
               [PublicReadBucket, AdminUsername, R2]).

create_group(Name) ->
    lager:info("creating group ~p", [Name]),
    case riak_core_security:add_group(Name, []) of
        ok ->
            lager:info("~p group created", [Name]);
        {error, role_exists} ->
            lager:info("~p group exists", [Name]);
        Other ->
            lager:warning("unknown response in group ~p creation '~p'",
                       [Name, Other])
    end.

create_groups() ->
    lists:foreach(fun (Group) -> create_group(Group) end,
                  ?ALL_GROUPS).
