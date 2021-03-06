-module(iorio_rhandler_smc).
-export([init/1, stop/1]).
-ignore_xref([init/1, stop/1]).
-behaviour(rscbag_resource_handler).

init(Opts) ->
    smc_hist_channel:start_link(Opts).

stop(Smc) ->
    try
        smc_hist_channel:stop(Smc)
    catch exit:{noproc, Info} ->
        lager:warning("Stoping smc hist channel: No Process ~p", [Info])
    end.
