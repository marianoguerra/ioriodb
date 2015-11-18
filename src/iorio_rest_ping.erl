-module(iorio_rest_ping).

-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_provided/2,
         malformed_request/2,
         options/2,
         to_json/2]).

-ignore_xref([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_provided/2,
         malformed_request/2,
         options/2,
         to_json/2]).

-record(state, {cors, access, iorio_state, creds}).

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest};
init({ssl, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{cors, Cors}, {iorio_state, IorioState}, {access, Access}]) ->
    {ok, Req, #state{cors=Cors, access=Access, iorio_state=IorioState}}.

options(Req, State=#state{cors=Cors}) ->
    Req1 = iorio_cors:handle_options(Req, ping, Cors),
    {ok, Req1, State}.

allowed_methods(Req, State) -> {[<<"OPTIONS">>, <<"GET">>], Req, State}.

malformed_request(Req, State=#state{access=Access}) ->
    iorio_http:check_rate_limit(ping, Access, Req, State,
                                fun (St, Creds) -> St#state{creds=Creds} end).

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, State=#state{iorio_state=IorioState}) ->
    {pong, Partition} = iorio:ping(IorioState),
    {iorio_json:encode([{pong, integer_to_binary(Partition)}]), Req, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.
