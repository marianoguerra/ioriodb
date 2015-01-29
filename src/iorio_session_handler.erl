-module(iorio_session_handler).

-export([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         is_authorized/2,
         resource_exists/2,
         from_json/2,
         to_json/2
        ]).

-include_lib("jwt/include/jwt.hrl").

-record(state, {secret, algorithm, session, session_duration_secs}).

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest};
init({ssl, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{secret, Secret}, {algorithm, Algorithm}, {session_duration_secs, SessionDurationSecs}]) ->
	{ok, Req, #state{secret=Secret, algorithm=Algorithm,
                     session_duration_secs=SessionDurationSecs}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

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
    {Method, Req0} = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            check_is_authorized(Req0, State);
        _ ->
            SetSession = fun (St, Sess) -> St#state{session=Sess} end,
            iorio_session:handle_is_authorized(Req, Secret, State, SetSession)
    end.

to_json(Req, State=#state{session={Username, _Session, _SecCtx}}) ->
    RespBody = [{username, Username}],
    {iorio_json:encode(RespBody), Req, State}.

from_json(Req, State=#state{secret=Secret, algorithm=Algorithm,
                            session_duration_secs=SessionDurationSecs,
                            session={_Username, SessionBody, _SecCtx}}) ->

    Expiration = jwt:now_secs() + SessionDurationSecs,
    {ok, Token} = jwt:encode(Algorithm, SessionBody, Secret,
                             [{exp, Expiration}]),
    ResultJson = [{ok, true}, {token, Token}],
    ResultJsonBin = iorio_json:encode(ResultJson),
    Req1 = cowboy_req:set_resp_body(ResultJsonBin, Req),
    {{true, <<"/session">>}, Req1, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.

%% Private API
check_is_authorized(Req, State) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    AuthInfo = iorio_json:decode_plist(Body),
    Username = proplists:get_value(<<"username">>, AuthInfo),
    Password = proplists:get_value(<<"password">>, AuthInfo),

    Source = [{ip, {127, 0, 0, 1}}],
    case riak_core_security:authenticate(Username, Password, Source) of
        {ok, Ctx} ->
            RespBody = [{u, Username}],
            Session = {Username, RespBody, Ctx},
            NewState = State#state{session=Session},
            {true, Req1, NewState};
        _ -> {{false, <<"jwt">>}, iorio_http:unauthorized(Req1), State}
    end.

