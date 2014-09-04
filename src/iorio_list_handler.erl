-module(iorio_list_handler).

-export([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2]).

-record(state, {bucket}).

init({tcp, http}, _Req, []) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {Bucket, Req1} = cowboy_req:binding(bucket, Req),
	{ok, Req1, #state{bucket=Bucket}}.

allowed_methods(Req, State) -> {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

unique(List) ->
    Set = sets:from_list(List),
    sets:to_list(Set).

to_json(Req, State=#state{bucket=Bucket}) ->
    {Status, Data} = case iorio:list(Bucket) of
                         {partial, _Reason, PartialData} -> {partial, PartialData};
                         {ok, _Data}=OkResp -> OkResp
                     end,
    ItemsNested = lists:map(fun ({_Partition, _Node, NodeData}) ->
                                    NodeData
                            end, Data),
    Items = lists:flatten(ItemsNested),
    UniqueItems = unique(Items),

    {jsx:encode([{status, Status}, {data, UniqueItems}]), Req, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.
