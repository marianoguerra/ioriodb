-module(iorio_mqtt_handler).
-behaviour(mqttl_handler).

-export([init/1, stop/1, terminate/2, info/2, timeout/2, ping/1, connect/1,
         disconnect/1, error/2, publish/2, subscribe/2, unsubscribe/2,
         login/2]).

-record(state, {username, send, bucket_separator}).

-include_lib("sblob/include/sblob.hrl").

init(Opts) ->
    {mqttl_send, Send} = proplists:lookup(mqttl_send, Opts),
    BucketSeparator = proplists:get_value(bucket_separator, Opts, <<"!">>),
    DefaultUsername = proplists:get_value(default_username, Opts, <<"mqtt">>),
    {ok, #state{send=Send, bucket_separator=BucketSeparator,
                username=DefaultUsername}}.

ping(State=#state{}) -> {ok, State}.
connect(State=#state{}) -> {ok, State}.
disconnect(State=#state{}) -> {ok, State}.

error(State=#state{}, Error) ->
    lager:error("Error ~p", [Error]),
    {ok, State}.

publish(State=#state{username=Username, bucket_separator=BucketSeparator},
        {Topic, Qos, Dup, Retain, MessageId, Payload}) ->
    lager:info("Publish ~p ~p ~p ~p ~p ~p",
               [Topic, Qos, Dup, Retain, MessageId, Payload]),
    {Bucket, Stream} = extract_bucket_and_stream(Topic, Username,
                                                 BucketSeparator),
    iorio:put(Bucket, Stream, Payload),
    {ok, State}.

subscribe(State=#state{username=Username, bucket_separator=BucketSeparator},
          Topics) ->
    Fun = fun ({TopicName, Qos}, {QosList, IState}) ->
                  OState = IState,
                  QosVal = Qos,
                  {Bucket, Stream} = extract_bucket_and_stream(TopicName,
                                                               Username,
                                                               BucketSeparator),
                  lager:info("Subscribe ~p ~p", [TopicName, Qos]),
                  iorio:subscribe(Bucket, Stream, self()),
                  {[QosVal|QosList], OState}
          end,
    R = lists:foldl(Fun, {[], State}, Topics),
    {QoSList, NewState} = R,
    {ok, QoSList, NewState}.

unsubscribe(State=#state{username=Username, bucket_separator=BucketSeparator}, Topics) ->
    Fun = fun (TopicName, IState) ->
                  OState = IState,
                  {Bucket, Stream} = extract_bucket_and_stream(TopicName,
                                                               Username,
                                                               BucketSeparator),
                  lager:info("Unsubscribe ~p", [TopicName]),
                  iorio:unsubscribe(Bucket, Stream, self()),
                  OState
          end,
    NewState = lists:foldl(Fun, State, Topics),
    {ok, NewState}.

timeout(State=#state{}, InactiveMs) ->
    lager:info("session was inactive for ~p ms", [InactiveMs]),
    {ok, State}.

info(State=#state{send=Send, bucket_separator=BucketSeparator},
     {entry, Bucket, Stream, #sblob_entry{timestamp=_Timestamp,
                                          seqnum=_SeqNum, data=Data}}) ->
    % XXX: generate a msg id
    Topic = topic_from_bucket_and_stream(Bucket, Stream, BucketSeparator),
    MsgId = 1,
    Send(MsgId, Topic, Data),
    {ok, State};

info(State=#state{}, Msg) ->
    lager:info("received unknown info message '~p'", [Msg]),
    {ok, State}.

stop(State=#state{}) ->
    lager:info("stop"),
    {ok, State}.

terminate(State=#state{}, Reason) ->
    lager:info("terminate ~p", [Reason]),
    {ok, State}.

login(State=#state{}, {Username, Password}) ->
    lager:info("login ~p", [Username, Password]),
    {ok, State#state{username=to_binary(Username)}}.

to_binary(Val) when is_binary(Val) -> Val;
to_binary(Val) when is_list(Val) -> list_to_binary(Val).

extract_bucket_and_stream(Topic, Username, Sep) when is_list(Topic) ->
    BTopic = list_to_binary(Topic),
    extract_bucket_and_stream(BTopic, Username, Sep);
extract_bucket_and_stream(Topic, Username, Sep) ->
    case binary:split(Topic, [Sep]) of
        [Stream] -> {Username, Stream};
        [Bucket, Stream] -> {Bucket, Stream};
        [Bucket|Tail] -> {Bucket, list_to_binary(Tail)}
    end.

topic_from_bucket_and_stream(Bucket, Stream, Sep) ->
    list_to_binary([Bucket, Sep, Stream]).
