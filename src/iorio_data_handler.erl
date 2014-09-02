-module(iorio_data_handler).

-export([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         to_json/2
        ]).

-record(state, {bucket, stream, from_sn, limit}).

-include_lib("sblob/include/sblob.hrl").

to_int_or(Bin, Default) ->
    Str = binary_to_list(Bin),
    case string:to_integer(Str) of
        {Int, ""} -> Int;
        _ -> Default
    end.

init({tcp, http}, _Req, []) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, []) ->
    {Bucket, Req1} = cowboy_req:binding(bucket, Req),
    {Stream, Req2} = cowboy_req:binding(stream, Req1),
    {FromSNStr, Req3} = cowboy_req:qs_val(<<"from">>, Req2, <<"">>),
    {LimitStr, Req4} = cowboy_req:qs_val(<<"limit">>, Req3, <<"1">>),

    FromSN = to_int_or(FromSNStr, nil),
    Limit = to_int_or(LimitStr, 1),

	{ok, Req4, #state{bucket=Bucket, stream=Stream, from_sn=FromSN, limit=Limit}}.

allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

sblob_to_json(#sblob_entry{seqnum=SeqNum, timestamp=Timestamp, data=_Data}) ->
    [{meta, [{seqnum, SeqNum}, {timestamp, Timestamp}]}].

to_json(Req, State=#state{bucket=Bucket, stream=Stream, from_sn=From, limit=Limit}) ->
    Blobs = iorio:get(Bucket, Stream, From, Limit),
    Items = lists:map(fun sblob_to_json/1, Blobs),

    {jsx:encode(Items), Req, State}.

from_json(Req, State=#state{bucket=Bucket, stream=Stream}) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    case jsx:is_json(Body) of
        true ->
            SblobEntry = iorio:put(Bucket, Stream, Body),
            ResultJson = sblob_to_json(SblobEntry),
            ResultJsonBin = jsx:encode(ResultJson),
            Req2 = cowboy_req:set_resp_body(ResultJsonBin, Req1),
            {true, Req2, State};
        false ->
            Req2 = cowboy_req:set_resp_body(<<"{\"type\": \"invalid-body\"}">>, Req1),
            {false, Req2, State}
    end.


rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.
