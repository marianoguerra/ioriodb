-module(iorio_cbuf).
-export([new/1, new/2, add/2, takewhile_reverse/2]).

% a kind of circular buffer that is only useful for iorio_channel to keep the
% last N events, it has a MinSize and a MaxSize to avoid calling sublist on
% every add call when the buffer is full, by default the buffer will grow to
% twice the MinSize before being cut to MinSize, the items are stored in
% reverse order to take advantage of cons and because it's the default access
% pattern (get the last N events).
% take into account that when the buffer has more than MinSize and you operate
% on it you may get more than MinSize items in response, we don't hide the
% elements after MinSize

new(MinSize) -> new(MinSize, MinSize * 2).

new(MinSize, MaxSize) when MinSize < MaxSize ->
    {MinSize, MaxSize, 0, []}.

add({MinSize, MaxSize, Size, Items}, Item) when Size >= MaxSize ->
    NewItems = lists:sublist(Items, MinSize),
    add({MinSize, MaxSize, MinSize, NewItems}, Item);

add({MinSize, MaxSize, Size, Items}, Item) ->
    {MinSize, MaxSize, Size + 1, [Item|Items]}.

takewhile_reverse({_MinSize, _MaxSize, _Size, Items}, Fun) ->
    lists:takewhile(Fun, Items).


