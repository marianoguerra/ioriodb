-module(iorio_hist_channel).
-behaviour(gen_server).

-export([start_link/0, start_link/1, subscribe/2, subscribe/3, unsubscribe/2,
         send/2, replay/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {buffer, channel, check_interval_ms=30000}).
-include_lib("sblob/include/sblob.hrl").

%% API

start_link() -> start_link(50).

start_link(BufferSize) ->
    gen_server:start_link(?MODULE, [BufferSize], []).

replay(Channel, Pid, FromSeqNum) ->
    gen_server:call(Channel, {replay, Pid, FromSeqNum}).

subscribe(Channel, Pid) ->
    subscribe(Channel, Pid, nil).

subscribe(Channel, Pid, FromSeqNum) ->
    gen_server:call(Channel, {subscribe, Pid, FromSeqNum}).

unsubscribe(Channel, Pid) ->
    gen_server:call(Channel, {unsubscribe, Pid}).

send(Channel, Event) ->
    gen_server:call(Channel, {send, Event}).

% TODO: stop?

%% Server implementation, a.k.a.: callbacks

init([BufferSize]) ->
    Buffer = iorio_cbuf:new(BufferSize),
    {ok, Channel} = iorio_channel:new(),
    State = #state{channel=Channel, buffer=Buffer},
    {ok, State, State#state.check_interval_ms}.

handle_call({subscribe, Pid, nil}, _From, State=#state{channel=Channel}) ->
    iorio_channel:subscribe(Channel, Pid),
    {reply, ok, State, State#state.check_interval_ms};

handle_call({subscribe, Pid, FromSeqNum}, _From, State=#state{channel=Channel, buffer=Buffer}) ->
    do_replay(Pid, FromSeqNum, Buffer),
    iorio_channel:subscribe(Channel, Pid),
    {reply, ok, State, State#state.check_interval_ms};

handle_call({unsubscribe, Pid}, _From, State=#state{channel=Channel}) ->
    iorio_channel:unsubscribe(Channel, Pid),
    {reply, ok, State, State#state.check_interval_ms};

handle_call({send, Event}, _From, State=#state{channel=Channel, buffer=Buffer}) ->
    NewBuffer = iorio_cbuf:add(Buffer, Event),
    NewState = State#state{buffer=NewBuffer},
    iorio_channel:send(Channel, Event),
    {reply, ok, NewState, State#state.check_interval_ms};

handle_call({replay, Pid, FromSeqNum}, _From, State=#state{buffer=Buffer}) ->
    do_replay(Pid, FromSeqNum, Buffer),
    {reply, ok, State, State#state.check_interval_ms}.

handle_cast(Msg, State) ->
    io:format("Unexpected handle cast message: ~p~n", [Msg]),
    {noreply, State}.

handle_info(timeout, State=#state{buffer=Buffer}) ->
    NewBuffer = iorio_cbuf:remove_percentage(Buffer, 0.5),
    NewBufferSize = iorio_cbuf:size(NewBuffer),
    NewState = State#state{buffer=NewBuffer},

    if
        NewBufferSize == 0 ->
            lager:info("channel buffer empty, stopping channel"),
            {stop, normal, NewState};
        true ->
            lager:debug("reduced channel buffer because of inactivity to ~p items",
                      [NewBufferSize]),
            {noreply, NewState, State#state.check_interval_ms}
    end;

handle_info(Msg, State) ->
    io:format("Unexpected handle info message: ~p~n",[Msg]),
    {noreply, State}.


terminate(_Reason, _Gblob) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private api

do_replay(Pid, FromSeqNum, Buffer) ->
    GtFromSeqNum = fun ({entry, _Bucket, _Stream, #sblob_entry{seqnum=SeqNum}}) ->
                           SeqNum > FromSeqNum
                   end,
    ToReplayReverse = iorio_cbuf:takewhile_reverse(Buffer, GtFromSeqNum),
    ToReplay = lists:reverse(ToReplayReverse),
    Pid ! ToReplay,
    ok.
