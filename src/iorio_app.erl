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

    {ok, AccessLogic} = iorioc_setup:setup_access(application:get_all_env(iorio)),
    iorio_rest:setup(AccessLogic, iorio, nil),

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

envd(Par, Def) -> env(iorio, Par, Def).

env(App, Par, Def) ->
    application:get_env(App, Par, Def).

