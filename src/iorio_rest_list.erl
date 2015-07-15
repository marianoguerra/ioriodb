-module(iorio_rest_list).

-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_provided/2,
         options/2,
         is_authorized/2,
         to_json/2]).

-ignore_xref([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_provided/2,
         options/2,
         is_authorized/2,
         to_json/2]).

-record(state, {access, info, bucket, cors, iorio_mod, iorio_state}).
-include("include/iorio.hrl").

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest};
init({ssl, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{access, Access}, {cors, Cors},
                {iorio_mod, IorioMod}, {iorio_state, IorioState}]) ->

    {Bucket, Req1} = cowboy_req:binding(bucket, Req, any),
    {ok, Info} = ioriol_access:new_req([{bucket, Bucket}]),
    {ok, Req1, #state{access=Access, info=Info, bucket=Bucket, cors=Cors,
                      iorio_mod=IorioMod, iorio_state=IorioState}}.

options(Req, State=#state{cors=Cors}) ->
    Req1 = iorio_cors:handle_options(Req, list, Cors),
    {ok, Req1, State}.

allowed_methods(Req, State) -> {[<<"OPTIONS">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

unique(List) ->
    Set = sets:from_list(List),
    sets:to_list(Set).

is_authorized(Req, State=#state{access=Access, bucket=Bucket, info=Info}) ->
    case iorio_session:fill_session(Req, Access, Info) of
        {ok, Req1, Info1} ->
            State1 = State#state{info=Info1},
            Action = ?PERM_BUCKET_LIST,
            case ioriol_access:is_authorized_for_bucket(Access, Info1, Action) of
                {ok, Info2} ->
                    {true, Req1, State1#state{info=Info2}};
                {error, Reason} ->
                    unauthorized_response(Req1, Bucket, Reason, State1)
            end;
        {error, Reason, Req1} ->
            unauthorized_response(Req1, Bucket, Reason, State)
    end.

response_to_json(Req, State, Response) ->
    {Status, Data} = case Response of
                         {partial, _Reason, PartialData} -> {partial, PartialData};
                         {ok, _Data}=OkResp -> OkResp
                     end,
    ItemsNested = lists:map(fun ({_Partition, _Node, NodeData}) ->
                                    NodeData
                            end, Data),
    Items = lists:flatten(ItemsNested),
    UniqueItems = unique(Items),

    {iorio_json:encode([{status, Status}, {data, UniqueItems}]), Req, State}.

to_json(Req, State=#state{bucket=any, iorio_mod=Iorio, iorio_state=IorioState}) ->
    response_to_json(Req, State, Iorio:list(IorioState));
to_json(Req, State=#state{bucket=Bucket, iorio_mod=Iorio, iorio_state=IorioState}) ->
    response_to_json(Req, State, Iorio:list(IorioState, Bucket)).

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.

%% private api
unauthorized_response(Req, Bucket, Reason, State) ->
    Req1 = iorio_http:no_permission(Req),
    lager:warning("unauthorized list request on bucket ~p: ~p",
                  [Bucket, Reason]),
    {{false, <<"jwt">>}, Req1, State}.
