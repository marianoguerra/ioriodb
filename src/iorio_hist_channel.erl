-module(iorio_hist_channel).
-behaviour(gen_server).

-export([new/0, new/1, subscribe/2, subscribe/3, unsubscribe/2, send/2, replay/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {buffer, channel}).
-include_lib("sblob/include/sblob.hrl").

%% API

new() -> new(50).

new(BufferSize) ->
    gen_server:start(?MODULE, [BufferSize], []).

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
    {ok, State}.

handle_call({subscribe, Pid, nil}, _From, State=#state{channel=Channel}) ->
    iorio_channel:subscribe(Channel, Pid),
    {reply, ok, State};

handle_call({subscribe, Pid, FromSeqNum}, _From, State=#state{channel=Channel, buffer=Buffer}) ->
    do_replay(Pid, FromSeqNum, Buffer),
    iorio_channel:subscribe(Channel, Pid),
    {reply, ok, State};

handle_call({unsubscribe, Pid}, _From, State=#state{channel=Channel}) ->
    iorio_channel:unsubscribe(Channel, Pid),
    {reply, ok, State};

handle_call({send, Event}, _From, State=#state{channel=Channel, buffer=Buffer}) ->
    NewBuffer = iorio_cbuf:add(Buffer, Event),
    NewState = State#state{buffer=NewBuffer},
    iorio_channel:send(Channel, Event),
    {reply, ok, NewState};

handle_call({replay, Pid, FromSeqNum}, _From, State=#state{buffer=Buffer}) ->
    do_replay(Pid, FromSeqNum, Buffer),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("Unexpected handle cast message: ~p~n", [Msg]),
    {noreply, State}.

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
