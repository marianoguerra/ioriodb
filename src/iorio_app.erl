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
    {ok, ApiSecret} = env(auth_secret),
    {ok, ApiAlgorithm} = env(auth_algorithm),

    AdminUsername = envd(admin_username, "admin"),
    {ok, AdminPassword} = env(admin_password),

    AnonUsername = envd(anon_username, "anonymous"),
    % default password since login in as anonymous is not that useful
    AnonPassword = envd(anon_password, <<"secret">>),

    SessionDurationSecs = envd(session_duration_secs, 3600),

    CorsInfo = init_cors_info(),

    N = envd(req_n, 3),
    W = envd(req_w, 3),
    Timeout = envd(req_timeout, 5000),

    AuthMod = envd(auth_mod, permiso_rcore),
    AuthModOpts0 = envd(auth_mod_opts, []),

    OnUserCreated = fun (Mod, State, #user{username=Username}) ->
                            ioriol_access:maybe_grant_bucket_ownership(Mod, State, Username),
                            Mod:user_join(State, Username, ?USER_GROUP)
                    end,
    AuthModOpts = [{user_created_cb, OnUserCreated}|AuthModOpts0],
    {ok, AccessLogic} = ioriol_access:new([{auth_mod, AuthMod},
                                           {auth_mod_opts, AuthModOpts},
                                           {secret, ApiSecret}]),

    CreateGroupsResult = create_groups(AccessLogic),
    lager:info("create groups ~p", [CreateGroupsResult]),

    GrantAdminUsers = fun (Username) ->
                              Res = ioriol_access:grant(AccessLogic, AdminUsername,
                                                        ?PERM_MAGIC_BUCKET, any,
                                                        ?PERM_ADMIN_USERS),
                              lager:info("assign admin users to ~p: ~p",
                                         [Username, Res]),
                              ioriol_access:maybe_grant_bucket_ownership(AccessLogic, AdminUsername)
                      end,
    create_user(AccessLogic, AdminUsername, AdminPassword, [?ADMIN_GROUP],
                GrantAdminUsers),
    create_user(AccessLogic, AnonUsername, AnonPassword, [],
                fun (_) -> ok end),

    setup_initial_permissions(AccessLogic, AdminUsername),

    BaseDispatchRoutes = [
               {"/listen", bullet_handler,
                [{handler, iorio_listen_handler}, {access, AccessLogic},
                 {cors, CorsInfo}]},
               {"/streams/:bucket", iorio_rest_list,
                [{access, AccessLogic},
                 {cors, CorsInfo}]},
               {"/streams/:bucket/:stream", iorio_rest_stream,
                [{access, AccessLogic}, {n, N}, {w, W}, {timeout, Timeout},
                 {cors, CorsInfo}]},
               {"/buckets/", iorio_rest_list,
                [{access, AccessLogic}, {cors, CorsInfo}]},
               {"/access/:bucket/", iorio_rest_access,
                [{access, AccessLogic}, {cors, CorsInfo}]},
               {"/access/:bucket/:stream", iorio_rest_access,
                [{access, AccessLogic}, {cors, CorsInfo}]},

               {"/sessions", iorio_rest_session,
                [{access, AccessLogic}, {algorithm, ApiAlgorithm},
                 {session_duration_secs, SessionDurationSecs},
                 {cors, CorsInfo}]},
               {"/users/", iorio_rest_user,
                [{access, AccessLogic}, {cors, CorsInfo}]},
               {"/ping", iorio_rest_ping, [{cors, CorsInfo}]},

               {"/x/:handler/[...]", iorio_rest_custom,
                [{access, AccessLogic}, {cors, CorsInfo}]}
    ],

    UserDispatchRoutes = envd(api_handlers,
                             [{"/ui/[...]", iorio_cowboy_static,
                               {priv_dir, iorio, "assets",
                                [{mimetypes, cow_mimetypes, all}]}}]),
    lager:info("configuring routes with following user provided routes: ~p",
               UserDispatchRoutes),
    DispatchRoutes = BaseDispatchRoutes ++ UserDispatchRoutes,

    Dispatch = cowboy_router:compile([{'_', DispatchRoutes}]),

    HttpEnabled = envd(http_enabled, true),
    ApiPort = envd(http_port, 8080),
    ApiAcceptors = envd(http_acceptors, 100),
    CowboyOpts = [{env, [{dispatch, Dispatch}]},
                  {onresponse, fun iorio_stats:cowboy_response_hook/4},
                  {middlewares, [iorio_stats, cowboy_router, iorio_cors, cowboy_handler]}],

    if HttpEnabled ->
            lager:info("http api enabled, starting"),
           {ok, _} = cowboy:start_http(http, ApiAcceptors, [{port, ApiPort}], CowboyOpts);
       true ->
           lager:info("http api disabled"),
           ok
    end,

    SecureEnabled = envd(https_enabled, false),
    SecureApiPort = envd(https_port, 8443),

    if
        SecureEnabled ->
            lager:info("https api enabled, starting"),
            SSLCACertPath = envd(https_cacert, notset),
            {ok, SSLCertPath} = env(https_cert),
            {ok, SSLKeyPath} = env(https_key),

            BaseSSLOpts = [{port, SecureApiPort}, {certfile, SSLCertPath},
                           {keyfile, SSLKeyPath}],

            SSLOpts = if SSLCACertPath == notset -> BaseSSLOpts;
                         true -> [{cacertfile, SSLCACertPath}|BaseSSLOpts]
                      end,

            {ok, _} = cowboy:start_https(https, ApiAcceptors, SSLOpts, CowboyOpts);
        true ->
            lager:info("https api disabled"),
            ok
    end,

    MqttEnabled = envd(mqtt_enabled, false),
    if MqttEnabled -> {ok, _MqttSupPid} = start_mqtt(AccessLogic);
       true -> lager:info("mqtt disabled"), ok
    end,

    ExtensionsLoadOk = load_extensions_configs(),
    if ExtensionsLoadOk -> ok;
       true ->
           lager:warning("Some extension's Config failed to load. This may"
                         " cause unexpected behaviour on those extensions")
    end,

    iorio_stats:init_metrics(),
    MetricsBucket = envd(metrics_bucket, <<"$sys">>),
    MetricsStream = envd(metrics_stream, <<"metrics">>),
    MetricsInterval = envd(metrics_interval_ms, 60000),
    {ok, _Tref} = iorio_stats:start_metric_sender(MetricsBucket, MetricsStream,
                                                  MetricsInterval),

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
    SysBucket = <<"$sys">>,
    R0 = ioriol_access:grant(AccessLogic, <<"*">>, SysBucket, any, "iorio.get"),
    lager:info("set read permissions to ~s to all: ~p", [SysBucket, R0]),
    R1 = ioriol_access:grant(AccessLogic, <<"*">>, PublicReadBucket, any, "iorio.get"),
    lager:info("set read permissions to ~s to all: ~p", [PublicReadBucket, R1]),
    R2 = ioriol_access:grant(AccessLogic, list_to_binary(AdminUsername),
                             PublicReadBucket, any, "iorio.put"),
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
                  ?ALL_GROUPS),
    % TODO: move this to permiso
    UserGroup = ?USER_GROUP,
    AdminGroup = list_to_binary(?ADMIN_GROUP),
    riak_core_security:alter_group(AdminGroup, [{"groups", [UserGroup]}]).

start_mqtt(Access) ->
    lager:info("mqtt enabled, starting"),
    Acceptors = envd(mqtt_acceptors, 100),
    MaxConnections = envd(mqtt_max_connections, 1024),
    Port = envd(mqtt_port, 1883),
    UserHandlerOpts = [{access, Access}],
	{ok, _} = ranch:start_listener(iorio_mqtt, Acceptors, ranch_tcp,
                                   [{port, Port},
                                    {max_connections, MaxConnections}],
                                   mqttl_protocol,
                                   [{handler_opts,
                                     [{handler, iorio_mqtt_handler},
                                      {user_handler_opts, UserHandlerOpts}]}]),
    mqttl_sup:start_link().

init_cors_info() ->
    {ok, Origins} = env(cors_origins),
    BaseHeaders = envd(cors_headers, []),
    {ok, MaxAge}  = env(cors_max_age_secs),
    {ok, Enabled} = env(cors_enabled),

    Headers = [<<"X-Session">>, <<"Content-Type">>|BaseHeaders],

    Opts = [{origins, Origins}, {headers, Headers}, {max_age, MaxAge},
            {enabled, Enabled}],

    iorio_cors:new(Opts).

env(Par) -> env(iorio, Par).
envd(Par, Def) -> env(iorio, Par, Def).

env(App, Par) ->
    application:get_env(App, Par).

env(App, Par, Def) ->
    application:get_env(App, Par, Def).

load_extensions_configs() ->
    ExtensionsLoadResult = iorio_x:load_configs(envd(extension, [])),
    FailedExntentionLoad = lists:filter(fun (ok) -> false; (_) -> true end,
                                        ExtensionsLoadResult),
    length(FailedExntentionLoad) == 0.

