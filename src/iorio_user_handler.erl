-module(iorio_user_handler).

-export([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         is_authorized/2,
         content_types_accepted/2,
         from_json/2]).

-record(state, {session, secret}).

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{secret, Secret}]) ->
	{ok, Req, #state{secret=Secret}}.

allowed_methods(Req, State) -> {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

is_authorized(Req, State=#state{secret=Secret}) ->
    SetSession = fun (St, Sess) -> St#state{session=Sess} end,
    Res = iorio_session:handle_is_authorized(Req, Secret, State, SetSession),
    {AuthOk, Req1, State1} = Res,
    {Username, _, _} = State1#state.session,

    case {AuthOk, Username} of
        % NOTE: for now only admin is authorized to create users
        {true, <<"admin">>} ->
            Res;
        {true, _} ->
            Req2 = iorio_http:response(<<"{\"type\": \"no-perm\"}">>, Req1),
            {{false, <<"jwt">>}, Req2, State};
        _ ->
            Res
    end.

from_json(Req, State) ->
    {ok, BodyRaw, Req1} = cowboy_req:body(Req),
    Body = jsx:decode(BodyRaw),
    Username = proplists:get_value(<<"username">>, Body),
    Password = proplists:get_value(<<"password">>, Body),
    {Ok, Req2} = create_user(Username, Password, Req1),
    {Ok, Req2, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.

%% private

create_user(undefined, undefined, Req) ->
    {false, iorio_http:response(<<"{\"type\": \"no-user-and-pass\"}">>, Req)};

create_user(undefined, _, Req) ->
    {false, iorio_http:response(<<"{\"type\": \"no-user\"}">>, Req)};

create_user(_, undefined, Req) ->
    {false, iorio_http:response(<<"{\"type\": \"no-pass\"}">>, Req)};

create_user(Username, Password, Req) ->
    lager:info("creating user '~s'", [Username]),
    case iorio_user:create(Username, Password) of
        ok ->
            {true, iorio_http:response(<<"{\"ok\": true}">>, Req)};
        {error, role_exists} ->
            lager:error("creating existing user '~s'", [Username]),
            {false, iorio_http:response(<<"{\"type\": \"user-exists\"}">>, Req)};
        {error, illegal_name_char} ->
            lager:error("creating user '~s'", [Username]),
            {false, iorio_http:response(<<"{\"type\": \"illegal-username\"}">>, Req)};
        Error ->
            lager:error("creating user '~s' ~p", [Username, Error]),
            {false, iorio_http:response(<<"{\"ok\": false}">>, Req)}
    end.

