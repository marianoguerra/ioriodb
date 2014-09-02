-module(iorio_listen_handler).

-export([init/4, stream/3, info/3, terminate/2]).

% exported for tests
-export([encode_ok/1, encode_error/2]).

-include_lib("sblob/include/sblob.hrl").
-include("priv/include/listener.hrl").

init(_Transport, Req, Opts, _Active) ->
    lager:info("listener init ~p~n", [Opts]),
    Iorio = proplists:get_value(iorio, Opts, iorio),
    {ok, Req, #state{channels=[], iorio=Iorio}}.

get_channel_args(Msg) ->
    {proplists:get_value(<<"bucket">>, Msg),
     proplists:get_value(<<"stream">>, Msg)}.

encode_ok(Id) ->
    IdBin = integer_to_binary(Id),
    <<"{\"ok\": true", ",\"id\":", IdBin/binary, "}">>.

handle_ping(_Msg, Id, Req, State) ->
    {reply, encode_ok(Id), Req, State}.

with_stream(Fn, Msg, Id, Req, State) ->
    {Body, NewState} = case get_channel_args(Msg) of
               {undefined, undefined} ->
                               {encode_error(<<"missing bucket and stream">>, Id), State};
               {undefined, _} ->
                               {encode_error(<<"missing bucket">>, Id), State};
               {_, undefined} ->
                               {encode_error(<<"missing stream">>, Id), State};
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
remove(V, [H|T], Accum) -> remove(V, T, [H|Accum]).

handle_subscribe(Msg, Id, Req, State=#state{channels=Channels, iorio=Iorio}) ->
    with_stream(fun (Bucket, Stream) ->
                        Key = {Bucket, Stream},
                        IsSubscribed = contains(Key, Channels),
                        if
                            IsSubscribed ->
                                lager:warning("already subscribed ~s/~s~n", [Bucket, Stream]),
                                {encode_error(<<"already subscribed">>, Id), State};

                            true ->
                                lager:info("subscribing ~s/~s~n", [Bucket, Stream]),
                                Iorio:subscribe(Bucket, Stream, self()),
                                NewChannels = [Key|Channels],
                                State1 = State#state{channels=NewChannels},
                                {encode_ok(Id), State1}
                        end
                end, Msg, Id, Req, State).


handle_unsubscribe(Msg, Id, Req, State=#state{channels=Channels, iorio=Iorio}) ->
    with_stream(fun (Bucket, Stream) ->
                        Key = {Bucket, Stream},
                        IsSubscribed = contains(Key, Channels),
                        if
                            IsSubscribed ->
                                lager:info("unsubscribing ~s/~s~n", [Bucket, Stream]),
                                Iorio:unsubscribe(Bucket, Stream, self()),
                                NewChannels = remove(Key, Channels),
                                State1 = State#state{channels=NewChannels},
                                {encode_ok(Id), State1};

                            true ->
                                lager:warning("not subscribed ~s/~s~n", [Bucket, Stream]),
                                {encode_error(<<"not subscribed">>, Id), State}

                        end
                end, Msg, Id, Req, State).

encode_error(Reason, Id) ->
    lager:warning("error ~s ~p~n", [Reason, Id]),
    IdBin = integer_to_binary(Id),
    <<"{\"ok\":false,\"id\":", IdBin, ",\"reason\":\"", Reason/binary, "\"}">>.

stream(Data, Req, State) ->
    lager:debug("msg received ~p~n", [Data]),
    Msg = jsx:decode(Data),
    Id = proplists:get_value(<<"id">>, Msg, 0),
    case proplists:get_value(<<"cmd">>, Msg) of
        <<"subscribe">> -> handle_subscribe(Msg, Id, Req, State);
        <<"unsubscribe">> -> handle_unsubscribe(Msg, Id, Req, State);
        <<"ping">> -> handle_ping(Msg, Id, Req, State);
        _ -> {reply, encode_error(<<"unknown command">>, Id), Req, State}
    end.

info({entry, BucketName, Stream, Entry}, Req, State) ->
    lager:debug("entry received ~s/~s~n", [BucketName, Stream]),
    Json = entry_to_json(Entry, BucketName, Stream),
    JsonBin = jsx:encode(Json),
    {reply, JsonBin, Req, State}.

terminate(_Req, #state{channels=Channels, iorio=Iorio}) ->
    lists:map(fun ({Bucket, Stream}) ->
                      lager:debug("unsubscribing from ~s/~s~n", [Bucket, Stream]),
                      Iorio:unsubscribe(Bucket, Stream, self())
              end, Channels),
    ok.

% private

entry_to_json(#sblob_entry{seqnum=SeqNum, timestamp=Timestamp, data=Data}, Bucket, Stream) ->
    [{meta, [{seqnum, SeqNum}, {timestamp, Timestamp}, {bucket, Bucket}, {stream, Stream}]},
     {data, jsx:decode(Data)}].
