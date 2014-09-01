-module(iorio_listen_handler).

-export([init/4, stream/3, info/3, terminate/2]).

-include_lib("sblob/include/sblob.hrl").

-record(state, {channels}).

init(_Transport, Req, Opts, _Active) ->
    io:format("listen init ~p~n", [Opts]),
    iorio:subscribe(<<"foo">>, <<"bar">>, self()),
    {ok, Req, #state{channels=[]}}.

stream(<<"ping">>, Req, State) ->
    io:format("ping received~n"),
    {reply, <<"pong">>, Req, State};

stream(Data, Req, State=#state{channels=_Channels}) ->
    io:format("message received ~s~n", [Data]),
    {reply, <<"[]">>, Req, State}.

info({entry, BucketName, Stream, Entry}, Req, State) ->
    io:format("msg received ~s/~s~n", [BucketName, Stream]),
    Json = entry_to_json(Entry, BucketName, Stream),
    JsonBin = jsx:encode(Json),
    {reply, JsonBin, Req, State}.

terminate(_Req, #state{}) ->
    io:format("unsubscribing from channels~n"),
    iorio:unsubscribe(<<"foo">>, <<"bar">>, self()),
    ok.

% private

entry_to_json(#sblob_entry{seqnum=SeqNum, timestamp=Timestamp, data=Data}, Bucket, Stream) ->
    [{meta, [{seqnum, SeqNum}, {timestamp, Timestamp}, {bucket, Bucket}, {stream, Stream}]},
     {data, jsx:decode(Data)}].
