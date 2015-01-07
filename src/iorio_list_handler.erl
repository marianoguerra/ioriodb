-module(iorio_list_handler).

-export([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_provided/2,
         is_authorized/2,
         to_json/2]).

-record(state, {bucket, secret, session=nil}).
-include("include/iorio.hrl").

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{secret, Secret}]) ->
    {Bucket, Req1} = cowboy_req:binding(bucket, Req, any),
	{ok, Req1, #state{bucket=Bucket, secret=Secret}}.

allowed_methods(Req, State) -> {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

unique(List) ->
    Set = sets:from_list(List),
    sets:to_list(Set).

get_session(#state{session=Session}) -> Session.
set_session(State, Session) -> State#state{session=Session}.

is_authorized(Req, State=#state{secret=Secret, bucket=Bucket}) ->
    Action = ?PERM_BUCKET_LIST,
    GetSession = fun get_session/1,
    SetSession = fun set_session/2,
    iorio_session:handle_is_authorized_for_bucket(Req, Secret, State,
                                                  GetSession, SetSession,
                                                  Bucket, Action).

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

to_json(Req, State=#state{bucket=any}) ->
    response_to_json(Req, State, iorio:list());
to_json(Req, State=#state{bucket=Bucket}) ->
    response_to_json(Req, State, iorio:list(Bucket)).

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.
