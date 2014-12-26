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

    GrantAdminUsers = fun (Username) ->
                              Res = iorio_session:grant(AdminUsername,
                                                        ?PERM_MAGIC_BUCKET, any,
                                                        ?PERM_ADMIN_USERS),
                              lager:info("assign admin users to ~p: ~p",
                                         [Username, Res])

                      end,
    create_user(AdminUsername, AdminPassword, GrantAdminUsers),
    create_user(AnonUsername, AnonPassword, fun (_) -> ok end),

    setup_initial_permissions(AdminUsername),

    Dispatch = cowboy_router:compile([
        {'_', [
               {"/listen", bullet_handler, [{handler, iorio_listen_handler}, {secret, ApiSecret}]},
               {"/streams/:bucket", iorio_list_handler, [{secret, ApiSecret}]},
               {"/streams/:bucket/:stream", iorio_stream_handler, [{secret, ApiSecret}]},
               {"/buckets/", iorio_list_handler, [{secret, ApiSecret}]},
               {"/access/:bucket/", iorio_access_handler, [{secret, ApiSecret}]},
               {"/access/:bucket/:stream", iorio_access_handler, [{secret, ApiSecret}]},

               {"/sessions", iorio_session_handler,
                [{secret, ApiSecret}, {algorithm, ApiAlgorithm},
                 {session_duration_secs, SessionDurationSecs}]},
               {"/users/", iorio_user_handler, [{secret, ApiSecret}]},
               {"/ping", iorio_ping_handler, []},

               {"/ui/[...]", cowboy_static, {priv_dir, iorio, "assets",
                                             [{mimetypes, cow_mimetypes, all}]}}
        ]}
    ]),
    ApiPort = application:get_env(iorio, port, 8080),
    ApiAcceptors = application:get_env(iorio, nb_acceptors, 100),
    {ok, _} = cowboy:start_http(http, ApiAcceptors, [{port, ApiPort}], [
        {env, [{dispatch, Dispatch}]}
    ]),

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

create_user(Username, Password, OnUserCreated) ->
    case iorio_user:create(Username, Password) of
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
