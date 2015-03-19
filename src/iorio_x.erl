-module(iorio_x).
-export([name_to_module/1, load_configs/1, get_config/1]).

name_to_module(Name) ->
    CompleteHandlerName = << <<"ioriox_">>/binary, Name/binary >>,
    try
        ModName = binary_to_existing_atom(CompleteHandlerName, utf8),
        {ok, ModName}
    catch
        error:badarg -> {error, {invalid_module, Name}}
    end.

load_configs(Configs) ->
    lists:map(fun ({Name, Path}) -> load_config(Name, Path) end, Configs).

load_config(Name, Path) ->
    AbsPath = filename:absname(Path),
    lager:info("Loading config for extension ~p at ~s", [Name, AbsPath]),
    case iorio_json:decode_file(AbsPath) of
        {ok, Data} ->
            application:set_env(iorio_extension, Name, Data),
            ok;
        Other ->
            lager:error("Error loading config for extension ~p: ~p",
                        [Name, Other]),
            Other
    end.

get_config(Name) ->
    application:get_env(iorio_extension, Name).
