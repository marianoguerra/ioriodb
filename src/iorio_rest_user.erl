-module(iorio_rest_user).

-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         is_authorized/2,
         resource_exists/2,
         content_types_accepted/2,
         content_types_provided/2,
         to_json/2,
         from_json/2]).

-ignore_xref([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         is_authorized/2,
         resource_exists/2,
         content_types_accepted/2,
         content_types_provided/2,
         to_json/2,
         from_json/2]).

-include("include/iorio.hrl").

-record(state, {access, info}).

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest};
init({ssl, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{access, Access}]) ->
    Bucket = ?PERM_MAGIC_BUCKET,
    {ok, Info} = ioriol_access:new_req([{bucket, Bucket}]),
	{ok, Req, #state{access=Access, info=Info}}.

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

is_authorized(Req, State=#state{access=Access, info=Info}) ->
     case iorio_session:fill_session(Req, Access, Info) of
         {ok, Req1, Info1} ->
             State1 = State#state{info=Info1},
             Action = ?PERM_ADMIN_USERS,

             case ioriol_access:is_authorized_for_bucket(Access, Info1, Action) of
                 {ok, Info2} ->
                     {true, Req1, State1#state{info=Info2}};
                 {error, Reason} ->
                     unauthorized_response(Req1, State1, Reason)
             end;
         {error, Reason, Req1} ->
             unauthorized_response(Req1, State, Reason)
     end.

action_from_req(Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    case Method of
        <<"POST">> -> {create_user, Req1};
        <<"PUT">> -> {update_user_password, Req1}
    end.

from_json(Req, State=#state{access=Access}) ->
    {ok, BodyRaw, Req1} = cowboy_req:body(Req),
    try
        Body = iorio_json:decode_plist(BodyRaw),
        Username = proplists:get_value(<<"username">>, Body),
        Password = proplists:get_value(<<"password">>, Body),

        {Action, Req2} = action_from_req(Req),
        {Ok, Req3} = do_action(Access, Username, Password, Req2, Action),
        {Ok, Req3, State}
    catch
        error:badarg -> {false, iorio_http:invalid_body(Req1), State}
    end.

to_json(Req, State=#state{access=Access}) ->
    {ok, Users} = ioriol_access:users(Access),
    UsersJson = lists:map(fun (Username) -> [{username, Username}] end,
                          Users),
    UsersJsonStr = iorio_json:encode(UsersJson),
    {UsersJsonStr, Req, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.

%% private

do_action(_Access, undefined, undefined, Req, _Action) ->
    {false, iorio_http:error(Req, <<"no-user-and-pass">>, <<"No username and password fields">>)};

do_action(_Access, undefined, _, Req, _Action) ->
    {false, iorio_http:error(Req, <<"no-user">>, <<"No username field">>)};

do_action(_Access, _, undefined, Req, _Action) ->
    {false, iorio_http:error(Req, <<"no-pass">>, <<"No password field">>)};

do_action(Access, Username, Password, Req, Action) ->
    lager:info("~p'ing user '~s'", [Action, Username]),
    case {Action, ioriol_access:Action(Access, Username, Password)} of
        {create_user, ok} ->
            ioriol_access:maybe_grant_bucket_ownership(Access, Username),
            UriStr = io_lib:format("/users/~s", [Username]),
            {{true, UriStr}, iorio_http:ok(Req)};
        {update_user_password, ok} ->
            {true, iorio_http:ok(Req)};
        {create_user, {error, duplicate}} ->
            lager:error("creating existing user '~s'", [Username]),
            {false, iorio_http:error(Req, <<"user-exists">>, <<"User already exists">>)};
        {_, {error, illegal_name_char}} ->
            lager:error("creating user '~s'", [Username]),
            {false, iorio_http:error(Req, <<"illegal-username">>, <<"Illegal Username">>)};
        {Action, Error} ->
            lager:error("~p'inguser '~s' ~p", [Action, Username, Error]),
            {false, iorio_http:error(Req, <<"unknown-error">>, <<"Unknown Error">>)}
    end.

unauthorized_response(Req, State, Reason) ->
    Req1 = iorio_http:no_permission(Req),
    lager:warning("unauthorized user op: ~p", [Reason]),
    {{false, <<"jwt">>}, Req1, State}.
