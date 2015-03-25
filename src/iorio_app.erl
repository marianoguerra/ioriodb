-module(iorio_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include_lib("iorioc/include/iorio.hrl").
-include_lib("permiso/include/permiso.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % TODO: see where to start it
    file_handle_cache:start_link(),
    % TODO: check here that secret is binary and algorigthm is a valid one
    {ok, ApiSecret} = env(auth_secret),

    AdminUsername = envd(admin_username, "admin"),
    {ok, AdminPassword} = env(admin_password),

    AnonUsername = envd(anon_username, "anonymous"),
    % default password since login in as anonymous is not that useful
    AnonPassword = envd(anon_password, <<"secret">>),

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

    iorio_rest:setup(AccessLogic),

    MqttEnabled = envd(mqtt_enabled, false),
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
    R1 = ioriol_access:grant(AccessLogic, <<"*">>, PublicReadBucket, any,
                             "iorio.get"),
    lager:info("set read permissions to ~s to all: ~p",
               [PublicReadBucket, R1]),
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

env(Par) -> env(iorio, Par).
envd(Par, Def) -> env(iorio, Par, Def).

env(App, Par) ->
    application:get_env(App, Par).

env(App, Par, Def) ->
    application:get_env(App, Par, Def).

