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

    AccessLogic = init_auth(),
    IorioMod = iorio,
    IorioState = nil,
    init_web(AccessLogic),
    init_mqtt(AccessLogic, IorioMod, IorioState),
    init_extensions(),
    init_metrics(IorioMod, IorioState),
    IorioMod:init().

stop(_State) ->
    ok.

%% private api

on_user_created(Mod, State, #user{username=Username}) ->
    case ioriol_access:maybe_grant_bucket_ownership(Mod, State, Username) of
        ok -> ok;
        Error ->
            lager:warning("grant bucket ownership result: ~p", [Error])
    end,
    Mod:user_join(State, Username, ?USER_GROUP).

new_access_logic() ->
    {ok, ApiSecret} = env(auth_secret),
    OnUserCreated = fun on_user_created/3,
    AuthMod = envd(auth_mod, permiso_rcore),
    AuthModOpts0 = envd(auth_mod_opts, []),
    AuthModOpts = [{user_created_cb, OnUserCreated}|AuthModOpts0],
    {ok, AccessLogic} = ioriol_access:new([{auth_mod, AuthMod},
                                           {auth_mod_opts, AuthModOpts},
                                           {secret, ApiSecret}]),
    AccessLogic.

grant_admin_perms(AccessLogic, Username) ->
    Res1 = ioriol_access:grant(AccessLogic, Username, ?PERM_MAGIC_BUCKET, any,
                               ?PERM_ADMIN_USERS),
    Res2 = ioriol_access:grant(AccessLogic, Username, any, any,
                             ?PERM_BUCKET_LIST),
    Res3 = ioriol_access:maybe_grant_bucket_ownership(AccessLogic, Username),
    lager:info("assign admin users to ~p: ~p", [Username, Res1]),
    lager:info("assign list buckets to ~p: ~p", [Username, Res2]),
    lager:info("assign bucket ownership to ~p: ~p", [Username, Res3]).

init_auth() ->
    AdminUsername = envd(admin_username, "admin"),
    {ok, AdminPassword} = env(admin_password),

    AnonUsername = envd(anon_username, "anonymous"),
    % default password since login in as anonymous is not that useful
    AnonPassword = envd(anon_password, <<"secret">>),

    AccessLogic = new_access_logic(),

    CreateGroupsResult = create_groups(AccessLogic),
    lager:info("create groups ~p", [CreateGroupsResult]),

    GrantAdminPerms = fun (Username) ->
                              grant_admin_perms(AccessLogic, Username)
                      end,
    create_user(AccessLogic, AdminUsername, AdminPassword, [?ADMIN_GROUP],
                GrantAdminPerms),
    create_user(AccessLogic, AnonUsername, AnonPassword, [],
                fun (_) -> ok end),

    setup_initial_permissions(AccessLogic, AdminUsername),
    AccessLogic.

rcs_check_permission(Ctx) ->
    Bucket = ?PERM_MAGIC_BUCKET,
    Action = ?PERM_ADMIN_USERS,
    riak_core_security:check_permission({Action, Bucket}, Ctx).

rcs_is_authorized(Access, Secret, Req, _RcsInfo) ->
    {ok, Session, Req1} = iorio_session:from_request(Access, Req, Secret),
    {_Username, _SBody, {ctx, UserCtx}} = Session,
    case rcs_check_permission(UserCtx) of
        {true, _Ctx1} ->
            {true, Req1};
        {false, Reason, _Ctx1} ->
            lager:warning("in rcs admin auth check ~p", [Reason]),
            {false, Req1}
    end.

rcs_is_user_authorized(_Access, Algorithm, Secret, SessionDurationSecs,
                       Req, #{user_ctx := UserCtx}) ->
    case rcs_check_permission(UserCtx) of
        {true, _Ctx1} ->
            Username = riak_core_security:get_username(UserCtx),
            Req1 = iorio_session:auth_ok_response(Req, Username, Algorithm,
                                                  Secret, SessionDurationSecs),
            {true, Req1};
        {false, Reason, _Ctx1} ->
            lager:warning("in rcs admin auth ~p", [Reason]),
            {false, Req}
    end.

base_routes(AccessLogic, CorsInfo) ->
    N = envd(req_n, 3),
    W = envd(req_w, 3),
    Timeout = envd(req_timeout, 5000),
    {ok, ApiAlgorithm} = env(auth_algorithm),
    {ok, ApiSecret} = env(auth_secret),
    SessionDurationSecs = envd(session_duration_secs, 3600),
    IorioMod = iorio,
    IorioState = nil,

    JsonEncoder = fun iorio_json:encode/1,
    JsonDecoder = fun iorio_json:decode/1,
    IsUserAuthorizedFun = fun (Req, Info) ->
                                  rcs_is_user_authorized(AccessLogic,
                                                         ApiAlgorithm,
                                                         ApiSecret,
                                                         SessionDurationSecs,
                                                         Req, Info)
                          end,
    IsAuthorizedFun = fun(Req, Info) ->
                              rcs_is_authorized(AccessLogic, ApiSecret, Req,
                                                Info)
                      end,
    RcsOpts = #{env_keys => [iorio],
                json_encoder => JsonEncoder, json_decoder => JsonDecoder,
                is_user_authorized => IsUserAuthorizedFun,
                base_uri => "/admin", is_authorized => IsAuthorizedFun},

    [{"/listen", bullet_handler,
      [{handler, iorio_listen_handler}, {access, AccessLogic},
       {cors, CorsInfo}, {iorio_mod, IorioMod}, {iorio_state, IorioState}]},
     {"/streams/:bucket", iorio_rest_list,
      [{access, AccessLogic}, {cors, CorsInfo},
       {iorio_mod, IorioMod}, {iorio_state, IorioState}]},
     {"/streams/:bucket/:stream", iorio_rest_stream,
      [{access, AccessLogic}, {n, N}, {w, W}, {timeout, Timeout},
       {cors, CorsInfo}, {iorio_mod, IorioMod}, {iorio_state, IorioState}]},
     {"/buckets/", iorio_rest_list,
      [{access, AccessLogic}, {cors, CorsInfo},
       {iorio_mod, IorioMod}, {iorio_state, IorioState}]},
     {"/access/:bucket/", iorio_rest_access,
      [{access, AccessLogic}, {cors, CorsInfo}]},
     {"/access/:bucket/:stream", iorio_rest_access,
      [{access, AccessLogic}, {cors, CorsInfo}]},

     {"/sessions", iorio_rest_session,
      [{access, AccessLogic}, {algorithm, ApiAlgorithm},
       {session_duration_secs, SessionDurationSecs},
       {cors, CorsInfo}]},
     {"/users/", iorio_rest_user, [{access, AccessLogic}, {cors, CorsInfo}]},
     {"/ping", iorio_rest_ping, [{cors, CorsInfo}, {iorio_mod, IorioMod},
                                 {iorio_state, IorioState}]},
     {"/admin/:action", rcs_cowboy_handler, RcsOpts},
     {"/admin/:action/:param1", rcs_cowboy_handler, RcsOpts},

     {"/x/:handler/[...]", iorio_rest_custom,
      [{access, AccessLogic}, {cors, CorsInfo},
       {iorio_mod, IorioMod}, {iorio_state, IorioState}]}].

user_routes() ->
    envd(api_handlers,
         [{"/ui/[...]", iorio_cowboy_static,
           {priv_dir, iorio, "assets",
            [{mimetypes, cow_mimetypes, all}]}}]).

api_middlewares(CorsInfo) ->
    CorsEnabled = iorio_cors:is_enabled(CorsInfo),
    if CorsEnabled ->
           lager:info("CORS enabled, adding middleware"),
           [iorio_stats, cowboy_router, iorio_cors, cowboy_handler];
       true ->
           lager:info("CORS disabled, not adding middleware"),
           [iorio_stats, cowboy_router, cowboy_handler]
    end.

web_routes(AccessLogic, CorsInfo) ->
    BaseDispatchRoutes = base_routes(AccessLogic, CorsInfo),
    UserDispatchRoutes = user_routes(),
    lager:info("configuring routes with following user provided routes: ~p",
               UserDispatchRoutes),
    DispatchRoutes = BaseDispatchRoutes ++ UserDispatchRoutes,
    cowboy_router:compile([{'_', DispatchRoutes}]).

init_web(AccessLogic) ->
    CorsInfo = init_cors_info(),
    Dispatch = web_routes(AccessLogic, CorsInfo),
    ApiMiddlewares = api_middlewares(CorsInfo),
    CowboyOpts = [{env, [{dispatch, Dispatch}]},
                  {onresponse, fun iorio_stats:cowboy_response_hook/4},
                  {middlewares, ApiMiddlewares}],

    init_http(CowboyOpts),
    init_https(CowboyOpts).

init_http(CowboyOpts) ->
    ApiAcceptors = envd(http_acceptors, 100),
    HttpEnabled = envd(http_enabled, true),
    ApiPort = envd(http_port, 8080),

    if HttpEnabled ->
            lager:info("http api enabled, starting"),
           {ok, _} = cowboy:start_http(http, ApiAcceptors, [{port, ApiPort}], CowboyOpts);
       true ->
           lager:info("http api disabled"),
           ok
    end.

init_https(CowboyOpts) ->
    SecureEnabled = envd(https_enabled, false),
    SecureApiPort = envd(https_port, 8443),
    ApiAcceptors = envd(http_acceptors, 100),

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
    end.

init_mqtt(AccessLogic, IorioMod, IorioState) ->
    MqttEnabled = envd(mqtt_enabled, false),
    if MqttEnabled -> {ok, _MqttSupPid} = start_mqtt(AccessLogic, IorioMod, IorioState);
       true -> lager:info("mqtt disabled"), ok
    end.

init_extensions() ->
    ExtensionsLoadOk = load_extensions_configs(),
    if ExtensionsLoadOk -> ok;
       true ->
           lager:warning("Some extension's Config failed to load. This may"
                         " cause unexpected behaviour on those extensions")
    end.

init_metrics(IorioMod, IorioState) ->
    iorio_stats:init_metrics(),
    MetricsBucket = envd(metrics_bucket, <<"$sys">>),
    MetricsStream = envd(metrics_stream, <<"metrics">>),
    MetricsInterval = envd(metrics_interval_ms, 60000),
    {ok, _Tref} = iorio_stats:start_metric_sender(IorioMod, IorioState,
                                                  MetricsBucket, MetricsStream,
                                                  MetricsInterval).


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
    PublicReadBucket = envd(public_bucket, <<"public">>),
    SysBucket = envd(metrics_bucket, <<"$sys">>),
    R0 = ioriol_access:group_grant(AccessLogic, ?ADMIN_GROUP, SysBucket, any,
                                   "iorio.get"),
    lager:info("set read permissions to ~s to admin group: ~p",
               [SysBucket, R0]),
    R1 = ioriol_access:group_grant(AccessLogic, ?USER_GROUP, PublicReadBucket,
                                   any, "iorio.get"),
    lager:info("set read permissions to ~s to all users: ~p", [PublicReadBucket, R1]),
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

start_mqtt(Access, IorioMod, IorioState) ->
    lager:info("mqtt enabled, starting"),
    Acceptors = envd(mqtt_acceptors, 100),
    MaxConnections = envd(mqtt_max_connections, 1024),
    Port = envd(mqtt_port, 1883),
    UserHandlerOpts = [{access, Access},
                       {iorio_mod, IorioMod}, {iorio_state, IorioState}],
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

