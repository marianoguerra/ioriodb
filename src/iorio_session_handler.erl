-module(iorio_session_handler).

-export([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         is_authorized/2,
         from_json/2,
         to_json/2
        ]).

-include_lib("jwt/include/jwt.hrl").

-record(state, {secret, algorithm, session}).

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{secret, Secret}, {algorithm, Algorithm}]) ->
	{ok, Req, #state{secret=Secret, algorithm=Algorithm}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

is_authorized(Req, State=#state{secret=Secret}) ->
    {Method, Req0} = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            {true, Req0, State};
        _ ->
            SetSession = fun (St, Sess) -> St#state{session=Sess} end,
            iorio_session:handle_is_authorized(Req, Secret, State, SetSession)
    end.

to_json(Req, State=#state{session={Username, _Session, _SecCtx}}) ->
    RespBody = [{username, Username}],
    {jsx:encode(RespBody), Req, State}.

from_json(Req, State=#state{secret=Secret, algorithm=Algorithm}) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    AuthInfo = jsx:decode(Body),
    Username = proplists:get_value(<<"username">>, AuthInfo),
    Password = proplists:get_value(<<"password">>, AuthInfo),

    Source = [{ip, {127, 0, 0, 1}}],
    ResultJson = case riak_core_security:authenticate(Username, Password, Source) of
                     {ok, _Ctx} ->
                         RespBody = [{u, Username}],
                         {ok, Token} = jwt:encode(Algorithm, RespBody, Secret),
                         [{ok, true}, {token, Token}];
                     _ ->
                         [{ok, false}]
                 end,

    ResultJsonBin = jsx:encode(ResultJson),
    Req2 = cowboy_req:set_resp_body(ResultJsonBin, Req1),
    {true, Req2, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.
