-module(iorio_mqtt_handler).
-behaviour(mqttl_handler).

-export([init/1, stop/1, terminate/2, info/2, timeout/2, ping/1, connect/1,
         disconnect/1, error/2, publish/2, subscribe/2, unsubscribe/2,
         login/2]).

-record(state, {username, send, bucket_separator, access, session}).

-include("include/iorio.hrl").
-include_lib("sblob/include/sblob.hrl").
-include_lib("mqttl/include/rabbit_mqtt_frame.hrl").

init(Opts) ->
    {mqttl_send, Send} = proplists:lookup(mqttl_send, Opts),
    BucketSeparator = proplists:get_value(bucket_separator, Opts, <<"!">>),
    DefaultUsername = proplists:get_value(default_username, Opts, <<"mqtt">>),
    {access, Access} = proplists:lookup(access, Opts),
    {ok, #state{send=Send, bucket_separator=BucketSeparator, access=Access,
                username=DefaultUsername}}.

ping(State=#state{}) -> {ok, State}.
connect(State=#state{}) -> {ok, State}.
disconnect(State=#state{}) -> {ok, State}.

error(State=#state{}, Error) ->
    lager:error("Error ~p", [Error]),
    {ok, State}.

publish(State=#state{username=Username, bucket_separator=BucketSeparator,
                     access=Access},
        {Topic, Qos, Dup, Retain, MessageId, Payload}) ->
    lager:debug("Publish ~p ~p ~p ~p ~p ~p",
               [Topic, Qos, Dup, Retain, MessageId, Payload]),
    {Bucket, Stream} = extract_bucket_and_stream(Topic, Username,
                                                 BucketSeparator),
    case get_session(State) of
        {ok, State1, Session} ->
            Perm = ?PERM_STREAM_PUT,
            CheckResult = ioriol_access:is_authorized_for_stream(Access,
                                                                 Session,
                                                                 Bucket,
                                                                 Stream, Perm),
            case CheckResult of
                ok ->
                    iorio:put(Bucket, Stream, Payload),
                    {ok, State1};
                {error, Reason} ->
                    {error, State1, Reason}
            end;
        {error, Reason} ->
            {error, State, Reason}
    end.

subscribe(State, Topics) ->
    case get_session(State) of
        {ok, State1, Session} ->
            subscribe(State1, Session, Topics);
        {error, Reason} ->
            {error, State, Reason}
    end.

unsubscribe(State=#state{username=Username, bucket_separator=BucketSeparator}, Topics) ->
    Fun = fun (TopicName, IState) ->
                  OState = IState,
                  {Bucket, Stream} = extract_bucket_and_stream(TopicName,
                                                               Username,
                                                               BucketSeparator),
                  lager:debug("Unsubscribe ~p", [TopicName]),
                  iorio:unsubscribe(Bucket, Stream, self()),
                  OState
          end,
    NewState = lists:foldl(Fun, State, Topics),
    {ok, NewState}.

timeout(State=#state{}, InactiveMs) ->
    lager:debug("session was inactive for ~p ms", [InactiveMs]),
    {ok, State}.

info(State=#state{}, Entry={entry, _Bucket, _Stream, #sblob_entry{}}) ->
    send(State, Entry),
    {ok, State};
info(State=#state{}, {replay, Entries}) ->
    SendEntry = fun (Entry) -> send(State, Entry) end,
    lists:foreach(SendEntry, Entries),
    {ok, State};
info(State=#state{}, {smc, {heartbeat, Props}}) ->
    lager:debug("smc heartbeat ~p", [Props]),
    {ok, State};
info(State=#state{}, {smc, Info}) ->
    lager:debug("channel update for ~p: ~p", [self(), Info]),
    {ok, State};
info(State=#state{}, Msg) ->
    lager:warning("received unknown info message '~p'", [Msg]),
    {ok, State}.

stop(State=#state{}) ->
    lager:debug("mqtt handler stop"),
    {ok, State}.

terminate(State=#state{}, Reason) ->
    lager:debug("mqtt handler terminate ~p", [Reason]),
    {ok, State}.

login(State, {Username, Password}) when is_list(Username) ->
    login(State, {list_to_binary(Username), Password});

login(State, {Username, Password}) when is_list(Password) ->
    login(State, {Username, list_to_binary(Password)});

login(State=#state{access=Access}, {Username, Password}) ->
    case ioriol_access:authenticate(Access, Username, Password) of
        {ok, Session} ->
            lager:debug("login ~p ok", [Username]),
            {ok, State#state{username=to_binary(Username), session=Session}};
        {error, Reason} ->
            lager:warning("login ~p failed: ~p", [Username, Reason]),
            {error, State, Reason}
    end.

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

%% Private

get_session(State=#state{session=undefined, access=Access, username=Username}) ->
    case ioriol_access:get_session(Access, Username) of
        {ok, Session} -> {ok, State#state{session=Session}, Session};
        Other -> Other
    end;
get_session(State=#state{session=Session}) ->
    {ok, State, Session}.


subscribe(State=#state{username=Username, bucket_separator=BucketSeparator,
                       access=Access}, Session, Topics) ->
    Fun = fun ({TopicName, Qos}, {QosList, IState}) ->
                  OState = IState,
                  {Bucket, Stream} = extract_bucket_and_stream(TopicName,
                                                               Username,
                                                               BucketSeparator),
                  Perm = ?PERM_STREAM_GET,
                  CheckResult = ioriol_access:is_authorized_for_stream(Access, Session,
                                                                       Bucket, Stream, Perm),
                  case CheckResult of
                      ok ->
                          % TODO
                          QosVal = ?QOS_0,
                          lager:debug("Subscribe ~p ~p", [TopicName, Qos]),
                          iorio:subscribe(Bucket, Stream, self()),
                          {[QosVal|QosList], OState};
                      {error, Reason} ->
                          lager:warning("Subscribe error ~p ~p ~p",
                                        [TopicName, Qos, Reason]),
                          QosVal = ?SUBACK_FAILURE,
                          {[QosVal|QosList], OState}
                  end
          end,
    R = lists:foldl(Fun, {[], State}, Topics),
    {QoSList, NewState} = R,
    {ok, QoSList, NewState}.

send(#state{send=Send, bucket_separator=BucketSeparator},
     {entry, Bucket, Stream, #sblob_entry{seqnum=_SeqNum, data=Data}}) ->
    Topic = topic_from_bucket_and_stream(Bucket, Stream, BucketSeparator),
    % XXX: generate a msg id
    MsgId = 1,
    Send(MsgId, Topic, Data).
