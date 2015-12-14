-module(iorio_vnode_channels).
-behaviour(gen_server).

-export([start_link/0, send/4, subscribe/5, unsubscribe/4, clean/1,
         send_metrics/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {channels}).
-record(metrics_state, {count=0}).

%% Public API

start_link() ->
    gen_server:start_link(?MODULE, [], []).

clean(Pid) ->
    gen_server:call(Pid, clean).

send_metrics(Pid) ->
    gen_server:call(Pid, send_metrics).

send(Pid, Bucket, Stream, Entry) ->
    gen_server:cast(Pid, {send, Bucket, Stream, Entry}).

subscribe(Pid, Bucket, Stream, FromSeqNum, Subscriber) ->
    gen_server:cast(Pid, {subscribe, Bucket, Stream, FromSeqNum, Subscriber}).

unsubscribe(Pid, Bucket, Stream, Subscriber) ->
    gen_server:cast(Pid, {unsubscribe, Bucket, Stream, Subscriber}).

%% Server implementation, a.k.a.: callbacks

init([]) ->
    ChansOpts = [{resource_handler, iorio_rhandler_smc}, {kv_mod, rscbag_ets}],
    {ok, Channels} = rscbag:init(ChansOpts),
    State = #state{channels=Channels},
    {ok, State}.

handle_call(clean, _From, State=#state{channels=Chans}) ->
    {ok, NewChans} = rscbag:clean(Chans),
    {reply, ok, State#state{channels=NewChans}};

handle_call(send_metrics, _From, State=#state{channels=Chans}) ->
    {ok, NewChans, Result} = rscbag:foldl(Chans, fun calculate_metrics/3,
                                          metrics_foldl_initial_state()),
    #metrics_state{count=ChanCount} = Result,
    iorio_stats:channel_count(ChanCount),
    {reply, ok, State#state{channels=NewChans}};

handle_call(Msg, _From, State) ->
    lager:warning("vnode_channels: Unexpected handle call message: ~p",[Msg]),
    {reply, ok, State}.


handle_cast({send, Bucket, Stream, Entry}, State=#state{channels=Chans}) ->
    NewChans = do_send(Chans, Bucket, Stream, Entry),
    {noreply, State#state{channels=NewChans}};

handle_cast({subscribe, Bucket, Stream, FromSeqNum, Subscriber}, State=#state{channels=Chans}) ->
    NewChans = do_subscribe(Chans, Bucket, Stream, FromSeqNum, Subscriber),
    {noreply, State#state{channels=NewChans}};

handle_cast({unsubscribe, Bucket, Stream, Subscriber}, State=#state{channels=Channels}) ->
    NewChans = do_unsubscribe(Channels, Bucket, Stream, Subscriber),
    {noreply, State#state{channels=NewChans}};

handle_cast(Msg, State) ->
    lager:warning("vnode_channels: Unexpected handle cast message: ~p",[Msg]),
    {noreply, State}.


handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State=#state{channels=Chans}) ->
    NewChans = remove_channel(Chans, Pid),
    iorio_stats:channel_destroy(),
    {noreply, State#state{channels=NewChans}};

handle_info(Msg, State) ->
    lager:warning("vnode_channels: Unexpected handle info message: ~p", [Msg]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private api

do_send(Chans, Bucket, Stream, Entry) ->
    {NewChans, Channel} = get_channel(Chans, Bucket, Stream),
    WasAlive = erlang:is_process_alive(Channel),

    if WasAlive -> ok;
       true -> lager:warning("Channel is dead ~p ~p", [Channel, WasAlive])
    end,

    try
        smc_hist_channel:send(Channel, {entry, Bucket, Stream, Entry}),
        NewChans
    catch T:E ->
              IsAlive = erlang:is_process_alive(Channel),
              lager:error("Error sending event to channel ~p/~p ~p ~p ~p alive: ~p/~p",
                          [Bucket, Stream, T, error_info(E), Channel, WasAlive, IsAlive]),
              NewChans
    end.

error_info({noproc, {gen_server, call, _}}) ->
    {noproc, {gen_server, call}};
error_info(Other) ->
    Other.

do_subscribe(Chans, Bucket, Stream, FromSeqNum, Pid) ->
    {NewChans, Channel} = get_channel(Chans, Bucket, Stream),
    check_channel(Channel),

    try
        smc_hist_channel:replay(Channel, Pid, FromSeqNum),
        smc_hist_channel:subscribe(Channel, Pid),
        NewChans
    catch T:E ->
              lager:error("Error subscribing to channel ~p/~p ~p ~p",
                          [Bucket, Stream, T, error_info(E),
                           erlang:is_process_alive(Channel)]),
              NewChans
    end.

do_unsubscribe(Chans, Bucket, Stream, Pid) ->
    ChannelKey = {Bucket, Stream},
    case rscbag:get_existing(Chans, ChannelKey) of
        {{error, notfound}, Chans1} ->
            lager:warning("unsubscribing for inexistent channel ~p/~p",
                          [Bucket, Stream]),
            Chans1;
        {{ok, _Cause, Channel}, Chans1} ->
            check_channel(Channel),

            try
                smc_hist_channel:unsubscribe(Channel, Pid),
                Chans1
            catch T:E ->
                      lager:error("Error unsubscribing to channel ~p/~p ~p ~p",
                                  [Bucket, Stream, T, error_info(E),
                                   erlang:is_process_alive(Channel)]),
                      Chans1
            end
    end.

get_channel(Channels, Bucket, Stream) ->
    ChannelKey = {Bucket, Stream},
    ChannelOptsFun = fun () -> new_channel_opts(Bucket, Stream) end,
    case rscbag:get(Channels, ChannelKey, ChannelOptsFun) of
        {{ok, created, Ch}, Channels1} ->
            erlang:monitor(process, Ch),
            iorio_stats:channel_create(),
            {Channels1, Ch};
        {{ok, found, Ch}, Channels1} ->
            {Channels1, Ch}
    end.

check_channel(Channel) ->
    IsAlive = erlang:is_process_alive(Channel),

    if IsAlive -> ok;
       true -> lager:warning("got dead channel ~p: ~p", [Channel, IsAlive])
    end.

remove_channel(Channels, Pid) ->
    case rscbag:remove_by_val(Channels, Pid, false) of
        {ok, NewChannels} ->
            NewChannels;
        {{error, Reason}, NewChannels} ->
            lager:warning("error removing channel: ~p ~p", [Pid, Reason]),
            NewChannels
    end.

new_channel_opts(Bucket, Stream) ->
    iorio_config:channel_config(Bucket, Stream).

metrics_foldl_initial_state() -> #metrics_state{}.

calculate_metrics({_Bucket, _Key}, Channel, State0=#metrics_state{count=Count}) ->
    try
        {ok, Status} = smc_hist_channel:status(Channel),

        SizeBytes = proplists:get_value(buf_size_bytes, Status),
        Size = proplists:get_value(buf_size, Status),
        SubCount = proplists:get_value(sub_count, Status),
        iorio_stats:channel_size_bytes(SizeBytes),
        iorio_stats:channel_size(Size),
        iorio_stats:channel_sub_count(SubCount),

        State0#metrics_state{count=Count + 1}
    catch T:E ->
        lager:warning("calculating channel metrics ~p: ~p", [T, E]),
        State0
    end.
