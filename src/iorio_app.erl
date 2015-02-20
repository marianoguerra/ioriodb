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
    {ok, ApiSecret} = env(iorio, secret),
    {ok, ApiAlgorithm} = env(iorio, algorithm),

    AdminUsername = env(iorio, admin_username, "admin"),
    {ok, AdminPassword} = env(iorio, admin_password),

    AnonUsername = env(iorio, anon_username, "anonymous"),
    % default password since login in as anonymous is not that useful
    AnonPassword = env(iorio, anon_password, <<"secret">>),

    SessionDurationSecs = env(iorio, session_duration_secs, 3600),

    N = env(iorio, req_n, 3),
    W = env(iorio, req_w, 3),
    Timeout = env(iorio, req_timeout, 5000),

    AccessHandler = env(iorio, access_handler, iorio_rk_access),
    {ok, AccessLogic} = ioriol_access:new([{handler, AccessHandler}, {secret, ApiSecret}]),

    create_groups(AccessLogic),

    GrantAdminUsers = fun (Username) ->
                              Res = ioriol_access:grant(AccessLogic, AdminUsername,
                                                        ?PERM_MAGIC_BUCKET, any,
                                                        ?PERM_ADMIN_USERS),
                              lager:info("assign admin users to ~p: ~p",
                                         [Username, Res]),
                              ioriol_access:maybe_grant_bucket_ownership(AccessLogic, AdminUsername)
                      end,
    create_user(AccessLogic, AdminUsername, AdminPassword, ?DEFAULT_ADMIN_GROUPS, GrantAdminUsers),
    create_user(AccessLogic, AnonUsername, AnonPassword, ?DEFAULT_ANONYMOUS_GROUPS, fun (_) -> ok end),

    setup_initial_permissions(AccessLogic, AdminUsername),

    BaseDispatchRoutes = [
               {"/listen", bullet_handler, [{handler, iorio_listen_handler}, {access, AccessLogic}]},
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

    UserDispatchRoutes = env(iorio, api_handlers, []),
    lager:info("configuring routes with following user provided routes: ~p",
               [UserDispatchRoutes]),
    DispatchRoutes = BaseDispatchRoutes ++ UserDispatchRoutes,

    Dispatch = cowboy_router:compile([{'_', DispatchRoutes}]),

    ApiPort = env(iorio, port, 8080),
    ApiAcceptors = env(iorio, nb_acceptors, 100),
    {ok, _} = cowboy:start_http(http, ApiAcceptors, [{port, ApiPort}],
                                [{env, [{dispatch, Dispatch}]}]),

    SecureEnabled = env(iorio, secure_enabled, false),
    SecureApiPort = env(iorio, secure_port, 8443),

    if
        SecureEnabled ->
            lager:info("secure api enabled, starting"),
            SSLCACertPath = env(iorio, secure_cacert, notset),
            {ok, SSLCertPath} = env(iorio, secure_cert),
            {ok, SSLKeyPath} = env(iorio, secure_key),

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

create_user(Access, Username, Password, Groups, OnUserCreated) ->
    case ioriol_access:create_user(Access, Username, Password, Groups) of
        ok ->
            lager:info("~p user created", [Username]),
            OnUserCreated(Username);
        {error, role_exists}  ->
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
        {error, role_exists} ->
            lager:info("~p group exists", [Name]);
        Other ->
            lager:warning("unknown response in group ~p creation '~p'",
                       [Name, Other])
    end.

create_groups(AccessLogic) ->
    lists:foreach(fun (Group) -> create_group(AccessLogic, Group) end,
                  ?ALL_GROUPS).

env(App, Par) ->
    application:get_env(App, Par).

env(App, Par, Def) ->
    application:get_env(App, Par, Def).
