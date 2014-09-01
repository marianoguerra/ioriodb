-module(iorio_listen_handler).

-export([init/4, stream/3, info/3, terminate/2]).

-include_lib("sblob/include/sblob.hrl").

-record(state, {channels, iorio}).

init(_Transport, Req, Opts, _Active) ->
    io:format("listen init ~p~n", [Opts]),
    {ok, Req, #state{channels=[], iorio=iorio}}.

get_channel_args(Msg) ->
    {proplists:get_value(<<"bucket">>, Msg),
     proplists:get_value(<<"stream">>, Msg)}.

handle_ping(_Msg, Req, State) ->
    {reply, <<"{\"ok\": true}">>, Req, State}.

with_stream(Fn, Msg, Req, State) ->
    {Body, NewState} = case get_channel_args(Msg) of
               {undefined, undefined} ->
                               {encode_error(<<"missing bucket and stream">>), State};
               {undefined, _} ->
                               {encode_error(<<"missing bucket">>), State};
               {_, undefined} ->
                               {encode_error(<<"missing stream">>), State};
               {Bucket, Stream} ->
                               Fn(Bucket, Stream)
           end,

    {reply, Body, Req, NewState}.

contains(_, []) -> false;
contains(H, [H|_]) -> true;
contains(V, [_|T]) -> contains(V, T).

remove(V, L) -> remove(V, L, []).

remove(_V, [], Accum) -> lists:reverse(Accum);
remove(V, [V|T], Accum) -> remove(V, T, Accum);
remove(V, [_|T], Accum) -> remove(V, T, [V|Accum]).

handle_subscribe(Msg, Req, State=#state{channels=Channels, iorio=Iorio}) ->
    with_stream(fun (Bucket, Stream) ->
                        Key = {Bucket, Stream},
                        IsSubscribed = contains(Key, Channels),
                        if
                            IsSubscribed ->
                               {encode_error(<<"already subscribed">>), State};

                            true ->
                                Iorio:subscribe(Bucket, Stream, self()),
                                NewChannels = [Key|Channels],
                                io:format("old ~p~nnew ~p~n", [Channels, NewChannels]),
                                State1 = State#state{channels=NewChannels},
                                {<<"{\"ok\": true}">>, State1}
                        end
                end, Msg, Req, State).


handle_unsubscribe(Msg, Req, State=#state{channels=Channels, iorio=Iorio}) ->
    with_stream(fun (Bucket, Stream) ->
                        Key = {Bucket, Stream},
                        IsSubscribed = contains(Key, Channels),
                        if
                            IsSubscribed ->
                                Iorio:unsubscribe(Bucket, Stream, self()),
                                NewChannels = remove(Key, Channels),
                                io:format("old ~p~nnew ~p~n", [Channels, NewChannels]),
                                State1 = State#state{channels=NewChannels},
                                {<<"{\"ok\": true}">>, State1};

                            true ->
                               {encode_error(<<"not subscribed">>), State}

                        end
                end, Msg, Req, State).

encode_error(Reason) ->
    <<"{\"ok\":false,\"reason\":\"", Reason/binary, "\"}">>.

stream(Data, Req, State) ->
    io:format("message received ~s~n", [Data]),
    Msg = jsx:decode(Data),
    case proplists:get_value(<<"cmd">>, Msg) of
        <<"subscribe">> -> handle_subscribe(Msg, Req, State);
        <<"unsubscribe">> -> handle_unsubscribe(Msg, Req, State);
        <<"ping">> -> handle_ping(Msg, Req, State);
        _ -> {reply, encode_error(<<"unknown command">>), Req, State}
    end.

info({entry, BucketName, Stream, Entry}, Req, State) ->
    io:format("msg received ~s/~s~n", [BucketName, Stream]),
    Json = entry_to_json(Entry, BucketName, Stream),
    JsonBin = jsx:encode(Json),
    {reply, JsonBin, Req, State}.

terminate(_Req, #state{channels=Channels, iorio=Iorio}) ->
    io:format("unsubscribing from channels~n"),
    lists:map(fun ({Bucket, Stream}) ->
                      Iorio:unsubscribe(Bucket, Stream, self())
              end, Channels),
    ok.

% private

entry_to_json(#sblob_entry{seqnum=SeqNum, timestamp=Timestamp, data=Data}, Bucket, Stream) ->
    [{meta, [{seqnum, SeqNum}, {timestamp, Timestamp}, {bucket, Bucket}, {stream, Stream}]},
     {data, jsx:decode(Data)}].
