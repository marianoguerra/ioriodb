-module(iorio_mqtt_handler).
-behaviour(mqttl_handler).

-export([init/1, stop/1, terminate/2, info/2, timeout/2, ping/1, connect/1,
         disconnect/1, error/2, publish/2, subscribe/2, unsubscribe/2,
         login/2]).

-record(state, {username = <<"_user_admin">>, send}).

-include_lib("sblob/include/sblob.hrl").

init([{mqttl_send, Send}]) -> {ok, #state{send=Send}}.
ping(State=#state{}) -> {ok, State}.
connect(State=#state{}) -> {ok, State}.
disconnect(State=#state{}) -> {ok, State}.

error(State=#state{}, Error) ->
    lager:error("Error ~p", [Error]),
    {ok, State}.

publish(State=#state{username=Username}, {Topic, Qos, Dup, Retain, MessageId, Payload}) ->
    lager:info("Publish ~p ~p ~p ~p ~p ~p",
               [Topic, Qos, Dup, Retain, MessageId, Payload]),
    iorio:put(Username, to_binary(Topic), Payload),
    {ok, State}.

subscribe(State=#state{username=Username}, Topics) ->
    R = lists:foldl(fun ({TopicName, Qos}, {QosList, IState}) ->
                            OState = IState,
                            QosVal = Qos,
                            lager:info("Subscribe ~p ~p", [TopicName, Qos]),
                            iorio:subscribe(Username, to_binary(TopicName),
                                            self()),
                            {[QosVal|QosList], OState}
                    end, {[], State}, Topics),
    {QoSList, NewState} = R,
    {ok, QoSList, NewState}.

unsubscribe(State=#state{username=Username}, Topics) ->
    NewState = lists:foldl(fun (TopicName, IState) ->
                            OState = IState,
                            lager:info("Unsubscribe ~p", [TopicName]),
                            iorio:unsubscribe(Username, to_binary(TopicName),
                                              self()),
                            OState
                    end, State, Topics),
    {ok, NewState}.

timeout(State=#state{}, InactiveMs) ->
    lager:info("session was inactive for ~p ms", [InactiveMs]),
    {ok, State}.

info(State=#state{send=Send}, {entry, _Bucket, Topic,
                               #sblob_entry{timestamp=_Timestamp,
                                            seqnum=_SeqNum, data=Data}}) ->
    % XXX: generate a msg id
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
