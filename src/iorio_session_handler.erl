-module(iorio_session_handler).

-export([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         to_json/2
        ]).

-include_lib("jwt/include/jwt.hrl").

-record(state, {secret, algorithm}).

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{secret, Secret}, {algorithm, Algorithm}]) ->
	{ok, Req, #state{secret=Secret, algorithm=Algorithm}}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

% TODO: use the cowboy rest callbacks instead of all here
to_json(Req, State=#state{secret=Secret}) ->
    {RespBody, Req1} = case cowboy_req:header(<<"x-session">>, Req) of
                           {undefined, R11} -> {[{ok, false}, {reason, nosession}], R11};
                           {JWTToken, R12} ->
                               case jwt:decode(JWTToken, Secret) of
                                   {ok, #jwt{body=BodyRaw}} ->
                                       Body = jsx:decode(BodyRaw),
                                       Username = proplists:get_value(<<"u">>, Body),
                                       {[{ok, true}, {username, Username}], R12};
                                   {error, Reason, _} ->
                                       {[{ok, false}, {reason, Reason}], R12}
                               end
                       end,

    {jsx:encode(RespBody), Req1, State}.

% TODO: use the cowboy rest callbacks instead of all here
from_json(Req, State=#state{secret=Secret, algorithm=Algorithm}) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    AuthInfo = jsx:decode(Body),
    Username = proplists:get_value(<<"username">>, AuthInfo),
    Password = proplists:get_value(<<"password">>, AuthInfo),

    % TODO implement auth
    ResultJson = if
                     Password == <<"secret">> ->
                         RespBody = [{u, Username}],
                         {ok, Token} = jwt:encode(Algorithm, RespBody, Secret),
                         [{ok, true}, {token, Token}];
                     true ->
                         [{ok, false}]
    end,

    ResultJsonBin = jsx:encode(ResultJson),
    Req2 = cowboy_req:set_resp_body(ResultJsonBin, Req1),
    {true, Req2, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.
