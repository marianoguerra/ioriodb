-module(lager_metrics).
-export([log_level/1, create/0, log_levels/0, log_level_key/1]).

-include_lib("lager/include/lager.hrl").

create() ->
    lists:map(fun create_log_level_metric/1, ?LEVELS).

log_levels() -> ?LEVELS.

log_level(Level) -> exometer:update(log_level_key(Level), 1).

log_level_key(Level) -> [iorio, core, log, Level].

create_log_level_metric(Level) ->
    exometer:new(log_level_key(Level), spiral, [{time_span, 60000}]).

