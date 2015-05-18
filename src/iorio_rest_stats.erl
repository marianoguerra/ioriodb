-module(iorio_rest_stats).

-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         is_authorized/2,
         content_types_provided/2,
         to_json/2
        ]).

-ignore_xref([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         is_authorized/2,
         content_types_provided/2,
         to_json/2
        ]).

-record(state, {access, info}).
-include("include/iorio.hrl").

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest};
init({ssl, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{access, Access}]) ->
    Bucket = ?PERM_MAGIC_BUCKET,
    {ok, Info} = ioriol_access:new_req([{bucket, Bucket}]),
    {ok, Req1, Info1} = iorio_session:fill_session(Req, Access, Info),
    {ok, Req1, #state{access=Access, info=Info1}}.

allowed_methods(Req, State) -> {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

is_authorized(Req, State=#state{access=Access, info=Info}) ->
    Action = ?PERM_ADMIN_USERS,

    case ioriol_access:is_authorized_for_bucket(Access, Info, Action) of
        {ok, Info1} ->
            {true, Req, State#state{info=Info1}};
        {error, Reason} ->
            Req1 = iorio_http:no_permission(Req),
            lager:warning("unauthorized stats op: ~p", [Reason]),
            {{false, <<"jwt">>}, Req1, State}
    end.

to_json(Req, State=#state{}) ->
    Stats = iorio_stats:all_stats(),
    {iorio_json:encode(Stats), Req, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.
