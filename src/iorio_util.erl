-module(iorio_util).
-export([reqid/0, log/2]).

reqid() -> make_ref().

log(info, Msg) -> lager:info("log: ~p", [Msg]);
log(warning, Msg) -> lager:warning("log: ~p", [Msg]);
log(error, Msg) -> lager:error("log: ~p", [Msg]);
log(debug, Msg) -> lager:debug("log: ~p", [Msg]).

