-module(iorio_rhandler_gblob_server).
-export([init/1, stop/1]).
-ignore_xref([init/1, stop/1]).
-behaviour(rscbag_resource_handler).

init(Opts) ->
    {path, Path} = proplists:lookup(path, Opts),
    GblobOpts = proplists:get_value(gblob_opts, Opts, []),
    GblobServerOpts = proplists:get_value(gblob_server_opts, Opts, []),
    gblob_server:start_link(Path, GblobOpts, GblobServerOpts).

stop(Pid) ->
    try
        gblob_server:stop(Pid)
    catch exit:{noproc, Info} ->
        lager:warning("Stoping gblob server: No Process ~p", [Info])
    end.

