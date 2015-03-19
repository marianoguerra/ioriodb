-module(iorio_json).

-export([decode/1, decode_plist/1, encode/1, is_json/1, decode_file/1]).

decode(Obj) -> jsx:decode(Obj, [return_maps]).
decode_plist(Obj) -> jsx:decode(Obj).
encode(Obj) -> jsx:encode(Obj).
is_json(Obj) -> jsx:is_json(Obj).

decode_file(Path) ->
    case file:read_file(Path) of
        {ok, Binary} ->
            try
                {ok, decode(Binary)}
            catch
                error:badarg -> {error, invalid_json}
            end;
        Other -> Other
    end.
