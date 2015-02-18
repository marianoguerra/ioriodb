-module(iorio_session_handler).

-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

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

-ignore_xref([rest_init/2,
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

-record(state, {access, info, algorithm, session_duration_secs}).

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest};
init({ssl, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{access, Access}, {algorithm, Algorithm},
                {session_duration_secs, SessionDurationSecs}]) ->

    {ok, Info} = ioriol_access:new_req([]),
	{ok, Req, #state{access=Access, info=Info, algorithm=Algorithm,
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

is_authorized(Req, State=#state{}) ->
    {Method, Req1} = cowboy_req:method(Req),
    case Method of
        <<"POST">> ->
            authenticate(Req1, State);
        _ ->
            check_is_authenticated(Req1, State)
    end.

to_json(Req, State=#state{info=Info}) ->
    Username = ioriol_access:username(Info),
    RespBody = [{username, Username}],
    {iorio_json:encode(RespBody), Req, State}.

from_json(Req, State=#state{access=Access, info=Info, algorithm=Algorithm,
                            session_duration_secs=SessionDurationSecs}) ->

    Secret = ioriol_access:secret(Access),
    SessionBody = ioriol_access:session_body(Info),
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

authenticate(Req, State=#state{info=Info, access=Access}) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    AuthInfo = iorio_json:decode_plist(Body),
    Username = proplists:get_value(<<"username">>, AuthInfo),
    Password = proplists:get_value(<<"password">>, AuthInfo),

    case ioriol_access:authenticate(Access, Info, Username, Password) of
        {ok, Info1} ->
            {true, Req1, State#state{info=Info1}};
        _Error ->
            {{false, <<"jwt">>}, iorio_http:unauthorized(Req1), State}
    end.

check_is_authenticated(Req, State=#state{info=Info, access=Access}) ->
    case iorio_session:fill_session(Req, Access, Info) of
        {ok, Req1, Info1} ->
            {true, Req1, State#state{info=Info1}};
        _Other ->
            {{false, <<"jwt">>}, iorio_http:no_permission(Req), State}
    end.
