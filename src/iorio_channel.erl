-module(iorio_channel).
-behaviour(gen_event).

-export([new/0, subscribe/2, unsubscribe/2, send/2]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).
%% API

new() -> gen_event:start_link().

subscribe(Channel, Pid) ->
    gen_event:add_handler(Channel, {iorio_channel, Pid}, [Pid]).

unsubscribe(Channel, Pid) ->
    gen_event:delete_handler(Channel, {iorio_channel, Pid}, [Pid]).

send(Channel, Event) ->
    gen_event:notify(Channel, Event).

-record(state, {pid}).

%% callbacks
init([Pid]) -> {ok, #state{pid=Pid}}.

handle_event(Msg, State=#state{pid=Pid}) ->
    Pid ! Msg,
    {ok, State}.

handle_call(_, State) -> {ok, ok, State}.

handle_info(_, State) -> {ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.
