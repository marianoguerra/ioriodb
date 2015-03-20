-module(iorio_rest_custom).

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

-export([behaviour_info/1]).

-ignore_xref([behaviour_info/1]).

-include("include/iorio.hrl").

-record(state, {access, handler_name, handler_state, handler}).

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

rest_init(Req, [{access, Access}]) ->
    {HandlerName, Req1} = cowboy_req:binding(handler, Req),
    % TODO: return 404 on error
    {ok, Handler} = iorio_x:name_to_module(HandlerName),
    {HandlerState, Req2} = Handler:init_req(Req1, Access),
	{ok, Req2, #state{access=Access, handler=Handler,
                      handler_name=HandlerName,
                      handler_state=HandlerState}}.

allowed_methods(Req, State=#state{handler_state=HState, handler=Handler}) ->
    {AllowedMethods, Req1} = Handler:allowed_methods(HState, Req),
    {AllowedMethods, Req1, State}.

% TODO
resource_exists(Req, State) ->
    {true, Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

% TODO
is_authorized(Req, State=#state{handler_state=HState, handler=Handler}) ->
    {Ok, Req1} = Handler:is_authorized(HState, Req),
    {Ok, Req1, State}.

from_json(Req, State=#state{handler_state=HState, handler=Handler}) ->
    {Method, Req1} = cowboy_req:method(Req),
    {ok, BodyRaw, Req2} = cowboy_req:body(Req1),
    % TODO: validate that it's json
    Body = iorio_json:decode(BodyRaw),
	{PathInfo, Req3} = cowboy_req:path_info(Req2),
	{Qs, Req4} = cowboy_req:qs_vals(Req3),
    case Handler:handle(HState, Req4, Method, PathInfo, Qs, Body) of
        {ok, {json, Response}, Req5} ->
            Req6 = iorio_http:json_response(Req5, Response),
            {ok, Req6, State};
        {ok, {raw, Response, ContentType}, Req5} ->
            Req6 = iorio_http:set_content_type_body(Req5, ContentType, Response),
            {ok, Req6, State};
        {error, Reason, Req5} ->
            Req6 = iorio_http:error(Req5, <<"error">>, Reason),
            {ok, Req6, State}
    end.

to_json(Req, State=#state{handler_state=HState, handler=Handler}) ->
    {Method, Req1} = cowboy_req:method(Req),
	{PathInfo, Req2} = cowboy_req:path_info(Req1),
	{Qs, Req3} = cowboy_req:qs_vals(Req2),
    case Handler:handle(HState, Req3, Method, PathInfo, Qs) of
        {ok, {json, Response}, Req4} ->
            ResponseJson = iorio_json:encode(Response),
            {ResponseJson, Req4, State};
        {ok, {raw, Response, ContentType}, Req4} ->
            Req5 = iorio_http:set_content_type(Req4, ContentType),
            {Response, Req5, State};
        {error, Reason, Req4} ->
            ResponseJson = iorio_json:encode([{type, <<"error">>},
                                              {reason, Reason}]),
            {ok, Req5} = cowboy_req:reply(400, [], ResponseJson, Req4),
            {halt, Req5, State}
    end.

rest_terminate(Req, #state{handler_state=HState, handler=Handler}) ->
    Handler:stop_req(HState, Req).

terminate(_Reason, Req, #state{handler_state=HState, handler=Handler}) ->
    Handler:terminate_req(HState, Req).

%% private
