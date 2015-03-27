-module(sblob_preg).
-export([new/0, put/3, get/2, get_reverse/2, remove/2, remove_reverse/2, foreach/2]).

new() -> ets:new(preg, [protected, ordered_set]).

put(Preg, Key, Val) ->
    ets:insert(Preg, {{key, Key}, Val}),
    ets:insert(Preg, {{val, Val}, Key}),
    Preg.

get(Preg, Key) ->
    ActualKey = {key, Key},
    case ets:lookup(Preg, ActualKey) of
        [{ActualKey, Val}] -> {value, Val};
        [] -> none
    end.

get_reverse(Preg, Val) ->
    ActualKey = {val, Val},
    case ets:lookup(Preg, ActualKey) of
        [{ActualKey, Key}] -> {key, Key};
        [] -> none
    end.

remove_both(Preg, Key, Val) ->
    ets:delete(Preg, {val, Val}),
    ets:delete(Preg, {key, Key}).

remove(Preg, Key) ->
    case get(Preg, Key) of
        {value, Val} ->
            remove_both(Preg, Key, Val);
        none ->
            ok
    end,
    Preg.

remove_reverse(Preg, Val) ->
    case get_reverse(Preg, Val) of
        {key, Key} ->
            remove_both(Preg, Key, Val);
        none ->
            ok
    end,
    Preg.

% iterates over all key values calling Fun for each pair, doesn't change the
% current value
foreach(Preg, Fun) ->
    Key = ets:first(Preg),
    foreach(Preg, Fun, Key).

foreach(Preg, _Fun, '$end_of_table') ->
    Preg;

foreach(Preg, Fun, Key={val, _Val}) ->
    NextKey = ets:next(Preg, Key),
    foreach(Preg, Fun, NextKey);

foreach(Preg, Fun, WholeKey={key, Key}) ->
    {value, Val} = get(Preg, Key),
    Fun(Key, Val),
    NextKey = ets:next(Preg, WholeKey),
    foreach(Preg, Fun, NextKey).

