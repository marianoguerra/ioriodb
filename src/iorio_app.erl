-module(iorio_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("include/iorio.hrl").
-include_lib("permiso/include/permiso.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % TODO: see where to start it
    file_handle_cache:start_link(),
    % TODO: check here that secret is binary and algorigthm is a valid one
    {ok, ApiSecret} = env(iorio, auth_secret),
    {ok, ApiAlgorithm} = env(iorio, auth_algorithm),

    AdminUsername = env(iorio, admin_username, "admin"),
    {ok, AdminPassword} = env(iorio, admin_password),

    AnonUsername = env(iorio, anon_username, "anonymous"),
    % default password since login in as anonymous is not that useful
    AnonPassword = env(iorio, anon_password, <<"secret">>),

    SessionDurationSecs = env(iorio, session_duration_secs, 3600),

    N = env(iorio, req_n, 3),
    W = env(iorio, req_w, 3),
    Timeout = env(iorio, req_timeout, 5000),

    AuthMod = env(iorio, auth_mod, permiso_rcore),
    AuthModOpts0 = env(iorio, auth_mod_opts, []),

    OnUserCreated = fun (Mod, State, #user{username=Username}) ->
                            ioriol_access:maybe_grant_bucket_ownership(Mod, State, Username),
                            lists:foreach(fun (Group) ->
                                                  Mod:user_join(State,
                                                                Username,
                                                                Group)
                                          end, ?DEFAULT_USER_GROUPS)
                    end,
    AuthModOpts = [{user_created_cb, OnUserCreated}|AuthModOpts0],
    {ok, AccessLogic} = ioriol_access:new([{auth_mod, AuthMod},
                                           {auth_mod_opts, AuthModOpts},
                                           {secret, ApiSecret}]),

    create_groups(AccessLogic),

    GrantAdminUsers = fun (Username) ->
                              Res = ioriol_access:grant(AccessLogic, AdminUsername,
                                                        ?PERM_MAGIC_BUCKET, any,
                                                        ?PERM_ADMIN_USERS),
                              lager:info("assign admin users to ~p: ~p",
                                         [Username, Res]),
                              ioriol_access:maybe_grant_bucket_ownership(AccessLogic, AdminUsername)
                      end,
    create_user(AccessLogic, AdminUsername, AdminPassword,
                ?DEFAULT_ADMIN_GROUPS, GrantAdminUsers),
    create_user(AccessLogic, AnonUsername, AnonPassword,
                ?DEFAULT_ANONYMOUS_GROUPS, fun (_) -> ok end),

    setup_initial_permissions(AccessLogic, AdminUsername),

    BaseDispatchRoutes = [
               {"/listen", bullet_handler, [{handler, iorio_listen_handler},
                                            {access, AccessLogic}]},
               {"/streams/:bucket", iorio_rest_list, [{access, AccessLogic}]},
               {"/streams/:bucket/:stream", iorio_rest_stream,
                [{access, AccessLogic}, {n, N}, {w, W}, {timeout, Timeout}]},
               {"/buckets/", iorio_rest_list, [{access, AccessLogic}]},
               {"/access/:bucket/", iorio_rest_access, [{access, AccessLogic}]},
               {"/access/:bucket/:stream", iorio_rest_access, [{access, AccessLogic}]},

               {"/sessions", iorio_rest_session,
                [{access, AccessLogic}, {algorithm, ApiAlgorithm},
                 {session_duration_secs, SessionDurationSecs}]},
               {"/users/", iorio_rest_user, [{access, AccessLogic}]},
               {"/ping", iorio_rest_ping, []}
    ],

    UserDispatchRoutes = env(iorio, api_handlers,
                             [{"/ui/[...]", iorio_cowboy_static,
                               {priv_dir, iorio, "assets",
                                [{mimetypes, cow_mimetypes, all}]}}]),
    lager:info("configuring routes with following user provided routes: ~p",
               UserDispatchRoutes),
    DispatchRoutes = BaseDispatchRoutes ++ UserDispatchRoutes,

    Dispatch = cowboy_router:compile([{'_', DispatchRoutes}]),

    ApiPort = env(iorio, http_port, 8080),
    ApiAcceptors = env(iorio, http_acceptors, 100),
    {ok, _} = cowboy:start_http(http, ApiAcceptors, [{port, ApiPort}],
                                [{env, [{dispatch, Dispatch}]}]),

    SecureEnabled = env(iorio, https_enabled, false),
    SecureApiPort = env(iorio, https_port, 8443),

    if
        SecureEnabled ->
            lager:info("https api enabled, starting"),
            SSLCACertPath = env(iorio, https_cacert, notset),
            {ok, SSLCertPath} = env(iorio, https_cert),
            {ok, SSLKeyPath} = env(iorio, https_key),

            BaseSSLOpts = [{port, SecureApiPort}, {certfile, SSLCertPath},
                           {keyfile, SSLKeyPath}],

            SSLOpts = if SSLCACertPath == notset -> BaseSSLOpts;
                         true -> [{cacertfile, SSLCACertPath}|BaseSSLOpts]
                      end,

            {ok, _} = cowboy:start_https(https, ApiAcceptors, SSLOpts,
                                         [{env, [{dispatch, Dispatch}]}]);
        true ->
            lager:info("https api disabled"),
            ok
    end,

    MqttEnabled = env(iorio, mqtt_enabled, false),
    if MqttEnabled -> {ok, _MqttSupPid} = start_mqtt(AccessLogic);
       true -> lager:info("mqtt disabled"), ok
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

create_user(Access, Username, Password, Groups, OnUserCreated) ->
    case ioriol_access:create_user(Access, Username, Password, Groups) of
        ok ->
            lager:info("~p user created", [Username]),
            OnUserCreated(Username);
        {error, duplicate}  ->
            lager:info("~p user exists", [Username]);
        OtherError ->
            lager:error("creating ~p user ~p", [Username, OtherError])
    end.

setup_initial_permissions(AccessLogic, AdminUsername) ->
    PublicReadBucket = <<"public">>,
    R1 = ioriol_access:grant(AccessLogic, <<"*">>, PublicReadBucket, any, "iorio.get"),
    lager:info("set read permissions to ~s to all: ~p",
               [PublicReadBucket, R1]),
    R2 = ioriol_access:grant(AccessLogic, list_to_binary(AdminUsername), PublicReadBucket,
                             any, "iorio.put"),
    lager:info("set write permissions to ~s to ~p: ~p",
               [PublicReadBucket, AdminUsername, R2]).

create_group(AccessLogic, Name) ->
    lager:info("creating group ~p", [Name]),
    case ioriol_access:add_group(AccessLogic, Name) of
        ok ->
            lager:info("~p group created", [Name]);
        {error, duplicate} ->
            lager:info("~p group exists", [Name]);
        Other ->
            lager:warning("unknown response in group ~p creation '~p'",
                       [Name, Other])
    end.

create_groups(AccessLogic) ->
    lists:foreach(fun (Group) -> create_group(AccessLogic, Group) end,
                  ?ALL_GROUPS).

start_mqtt(Access) ->
    lager:info("mqtt enabled, starting"),
    Acceptors = env(iorio, mqtt_acceptors, 100),
    MaxConnections = env(iorio, mqtt_max_connections, 1024),
    Port = env(iorio, mqtt_port, 1883),
    UserHandlerOpts = [{access, Access}],
	{ok, _} = ranch:start_listener(iorio_mqtt, Acceptors, ranch_tcp,
                                   [{port, Port},
                                    {max_connections, MaxConnections}],
                                   mqttl_protocol,
                                   [{handler_opts,
                                     [{handler, iorio_mqtt_handler},
                                      {user_handler_opts, UserHandlerOpts}]}]),
    mqttl_sup:start_link().

env(App, Par) ->
    application:get_env(App, Par).

env(App, Par, Def) ->
    application:get_env(App, Par, Def).

