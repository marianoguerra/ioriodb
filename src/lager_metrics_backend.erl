-module(lager_metrics_backend).
-behaviour(gen_event).

-export([init/1]).
-export([handle_call/2]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(s, {level}).

%%%_ * gen_event callbacks ---------------------------------------------
init(Opts) ->
  Level = proplists:get_value(level, Opts, info),
  {ok, #s{level=lager_util:level_to_num(Level)}}.

terminate(_Rsn, _S) ->
  ok.

handle_call({set_loglevel, Level}, S) ->
  {ok, ok, S#s{level=lager_util:level_to_num(Level)}};
handle_call(get_loglevel, S) ->
  {ok, S#s.level, S}.

%% lager 2.x
handle_event({log, Msg}, S) ->
  case lager_util:is_loggable(Msg, S#s.level, {?MODULE, ?MODULE}) of
    true  ->
          iorio_stats:log_level(lager_msg:severity(Msg)),
          {ok, S};
    false -> {ok, S}
  end.

handle_info(_Msg, S) ->
  {ok, S}.

code_change(_OldVsn, S, _Extra) ->
  {ok, S}.
