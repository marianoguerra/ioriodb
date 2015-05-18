-module(iorio_rest_ping).

-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2
        ]).

-ignore_xref([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2
        ]).

-record(state, {}).

init({tcp, http}, _Req, []) -> {upgrade, protocol, cowboy_rest};
init({ssl, http}, _Req, []) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) -> {ok, Req, #state{}}.

allowed_methods(Req, State) -> {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

to_json(Req, State=#state{}) ->
    {pong, Partition} = iorio:ping(),
    {iorio_json:encode([{pong, integer_to_binary(Partition)}]), Req, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.
