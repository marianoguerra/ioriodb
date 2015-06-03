-module(iorio_rest_custom).
% XXX: for now we don't enable cors on custom handlers until they are stabilized

-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         is_authorized/2,
         content_types_accepted/2,
         content_types_provided/2,
         known_methods/2,
         to_json/2,
         from_json/2,
         to_raw/2,
         from_raw/2]).

-ignore_xref([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         is_authorized/2,
         content_types_accepted/2,
         content_types_provided/2,
         known_methods/2,
         to_json/2,
         from_json/2,
         to_raw/2,
         from_raw/2]).

-export([behaviour_info/1]).

-ignore_xref([behaviour_info/1]).

-define(KNOWN_METHODS, [<<"GET">>, <<"HEAD">>, <<"POST">>, <<"PUT">>,
                        <<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>]).

-include("include/iorio.hrl").

-record(state, {access, handler_name, handler_state, handler, cors}).

behaviour_info(callbacks) ->
    [{init_req, 2},
     {stop_req, 2},
     {terminate_req, 2},

     {allowed_methods, 2},
     {is_authorized, 2},

     {handle, 5},
     {handle, 6}];

behaviour_info(_Other) ->
    undefined.

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest};
init({ssl, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{access, Access}, {cors, Cors}]) ->
    {HandlerName, Req1} = cowboy_req:binding(handler, Req),
	{ok, Req1, #state{access=Access, handler_name=HandlerName, cors=Cors}}.

known_methods(Req, State=#state{handler_name=HandlerName, access=Access}) ->
    case iorio_x:name_to_module(HandlerName) of
        {ok, Handler} ->
            {HandlerState, Req1} = Handler:init_req(Req, Access),
            {?KNOWN_METHODS, Req1, State#state{handler=Handler, handler_state=HandlerState}};
        {error, {invalid_module, _Name}} ->
            {[], Req, State}
    end.

allowed_methods(Req, State=#state{handler_state=HState, handler=Handler}) ->
    {AllowedMethods, Req1} = Handler:allowed_methods(HState, Req),
    {AllowedMethods, Req1, State}.

content_types_accepted(Req, State=#state{handler=Handler}) ->
    Json = {{<<"application">>, <<"json">>, '*'}, from_json},
    SupportsRaw = erlang:function_exported(Handler, handle_raw, 6),
    if SupportsRaw -> {[Json, {'*', from_raw}, {[], from_raw}],
                       Req, State};
       true -> {[Json], Req, State}
    end.

content_types_provided(Req, State=#state{handler=Handler}) ->
    Json = {{<<"application">>, <<"json">>, '*'}, to_json},
    SupportsRaw = erlang:function_exported(Handler, handle_raw, 5),
    if SupportsRaw -> {[Json, {'*', to_raw}, {[], to_raw}], Req, State};
       true -> {[Json], Req, State}
    end.

is_authorized(Req, State=#state{handler_state=HState, handler=Handler}) ->
    {Ok, Req1} = Handler:is_authorized(HState, Req),
    {Ok, Req1, State}.

from_json(Req, State) ->
    {Req1, Method, BodyRaw, PathInfo, Qs} = get_body_req_info(Req),
    try
        Body = iorio_json:decode(BodyRaw),
        handle(State, handle, Req1, Method, Body, PathInfo, Qs)
    catch
        error:badarg ->
            reply_error(Req1, State, <<"invalid-json">>)
    end.

to_json(Req, State) ->
    {Req1, Method, PathInfo, Qs} = get_req_info(Req),
    handle(State, handle, Req1, Method, PathInfo, Qs).

from_raw(Req, State) ->
    {Req1, Method, BodyRaw, PathInfo, Qs} = get_body_req_info(Req),
    handle(State, handle_raw, Req1, Method, BodyRaw, PathInfo, Qs).

to_raw(Req, State) ->
    {Req1, Method, PathInfo, Qs} = get_req_info(Req),
    handle(State, handle_raw, Req1, Method, PathInfo, Qs).

rest_terminate(_Req, #state{handler=undefined}) ->
    ok;
rest_terminate(Req, #state{handler_state=HState, handler=Handler}) ->
    Handler:stop_req(HState, Req).

terminate(_Reason, Req, #state{handler_state=HState, handler=Handler}) ->
    Handler:terminate_req(HState, Req).

%% private

get_req_info(Req) ->
    {Method, Req1} = cowboy_req:method(Req),
	{PathInfo, Req2} = cowboy_req:path_info(Req1),
	{Qs, Req3} = cowboy_req:qs_vals(Req2),

    {Req3, Method, PathInfo, Qs}.

get_body_req_info(Req) ->
    {Req1, Method, PathInfo, Qs} = get_req_info(Req),
    {ok, BodyRaw, Req2} = cowboy_req:body(Req1),
    {Req2, Method, BodyRaw, PathInfo, Qs}.

reply_error(Req, State, Reason) ->
    ResponseJson = iorio_json:encode([{type, <<"error">>}, {reason, Reason}]),
    {ok, Req1} = cowboy_req:reply(400, [], ResponseJson, Req),
    {halt, Req1, State}.

handle(State=#state{handler_state=HState, handler=Handler},
       FunName, Req, Method, PathInfo, Qs) ->
    case Handler:FunName(HState, Req, Method, PathInfo, Qs) of
        {ok, {json, Response}, Req2} ->
            ResponseJson = iorio_json:encode(Response),
            {ResponseJson, Req2, State};
        {ok, {raw, Response, ContentType}, Req2} ->
            Req3 = iorio_http:set_content_type(Req2, ContentType),
            {Response, Req3, State};
        {error, Reason, Req2} ->
            reply_error(Req2, State, Reason)
    end.

handle(State=#state{handler_state=HState, handler=Handler},
       FunName, Req, Method, Body, PathInfo, Qs) ->
    case Handler:FunName(HState, Req, Method, PathInfo, Qs, Body) of
        {ok, {json, Response}, Req1} ->
            Req6 = iorio_http:json_response(Req1, Response),
            {true, Req6, State};
        {ok, {raw, Response, ContentType}, Req1} ->
            Req6 = iorio_http:set_content_type_body(Req1, ContentType, Response),
            {true, Req6, State};
        {error, Reason, Req1} ->
            Req6 = iorio_http:error(Req1, <<"error">>, Reason),
            {false, Req6, State}
    end.

