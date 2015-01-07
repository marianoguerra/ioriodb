-module(iorio_user_handler).

-export([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         is_authorized/2,
         resource_exists/2,
         content_types_accepted/2,
         content_types_provided/2,
         to_json/2,
         from_json/2]).

-include("include/iorio.hrl").

-record(state, {session, secret}).

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{secret, Secret}]) ->
	{ok, Req, #state{secret=Secret}}.

allowed_methods(Req, State) -> {[<<"POST">>, <<"PUT">>, <<"GET">>], Req, State}.

resource_exists(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    Exists = case Method of
                 <<"POST">> -> false;
                 _ -> true
             end,
    {Exists, Req1, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

is_authorized(Req, State=#state{secret=Secret}) ->
    GetSession = fun (#state{session=Session}) -> Session end,
    SetSession = fun (St, Sess) -> St#state{session=Sess} end,
    Res = iorio_session:handle_is_authorized_for_bucket(Req, Secret, State,
                                                        GetSession, SetSession,
                                                        ?PERM_MAGIC_BUCKET,
                                                        ?PERM_ADMIN_USERS),
    {AuthOk, Req1, State1} = Res,

    if AuthOk -> Res;
        true ->
            Req2 = iorio_http:no_permission(Req1),
            {{false, <<"jwt">>}, Req2, State1}
    end.

action_from_req(Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    case Method of
        <<"POST">> -> {create, Req1};
        <<"PUT">> -> {update, Req1}
    end.

from_json(Req, State) ->
    {ok, BodyRaw, Req1} = cowboy_req:body(Req),
    try
        Body = iorio_json:decode_plist(BodyRaw),
        Username = proplists:get_value(<<"username">>, Body),
        Password = proplists:get_value(<<"password">>, Body),

        {Action, Req2} = action_from_req(Req),
        {Ok, Req3} = do_action(Username, Password, Req2, Action),
        {Ok, Req3, State}
    catch
        error:badarg -> {false, iorio_http:invalid_body(Req1), State}
    end.

to_json(Req, State) ->
    Users = iorio_user:users(),
    UsersJson = lists:map(fun ({Username, _}) -> [{username, Username}] end,
                          Users),
    UsersJsonStr = iorio_json:encode(UsersJson),
    {UsersJsonStr, Req, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.

%% private

do_action(undefined, undefined, Req, _Action) ->
    {false, iorio_http:error(Req, <<"no-user-and-pass">>, <<"No username and password fields">>)};

do_action(undefined, _, Req, _Action) ->
    {false, iorio_http:error(Req, <<"no-user">>, <<"No username field">>)};

do_action(_, undefined, Req, _Action) ->
    {false, iorio_http:error(Req, <<"no-pass">>, <<"No password field">>)};

do_action(Username, Password, Req, Action) ->
    lager:info("~p'ing user '~s'", [Action, Username]),
    case {Action, iorio_user:Action(Username, Password)} of
        {create, ok} ->
            iorio_session:maybe_grant_bucket_ownership(Username),
            UriStr = io_lib:format("/users/~s", [Username]),
            {{true, UriStr}, iorio_http:ok(Req)};
        {update, ok} ->
            {true, iorio_http:ok(Req)};
        {create, {error, role_exists}} ->
            lager:error("creating existing user '~s'", [Username]),
            {false, iorio_http:error(Req, <<"user-exists">>, <<"User already exists">>)};
        {_, {error, illegal_name_char}} ->
            lager:error("creating user '~s'", [Username]),
            {false, iorio_http:error(Req, <<"illegal-username">>, <<"Illegal Username">>)};
        {Action, Error} ->
            lager:error("~p'inguser '~s' ~p", [Action, Username, Error]),
            {false, iorio_http:error(Req, <<"unknown-error">>, <<"Unknown Error">>)}
    end.

