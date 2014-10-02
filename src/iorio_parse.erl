-module(iorio_parse).
-export([subscription/1, subscriptions/1]).

bin_to_int(Bin) ->
    try
        {ok, binary_to_integer(Bin)}
    catch
        error:badarg -> {error, badarg}
    end.

subscription(Sub) ->
    Parts = binary:split(Sub, [<<":">>], [global]),
    case Parts of
        [Bucket, Stream] ->
            {ok, {Bucket, Stream}};
        [Bucket, Stream, FromSeqNumBin] ->
            case bin_to_int(FromSeqNumBin) of
                {ok, FromSeqNum} -> {ok, {Bucket, Stream, FromSeqNum}};
                _ -> {error, badseqnum}
            end;
        _ -> {error, badarg}
    end.

subscriptions(Subs) ->
    Parse = fun (Sub) ->
                    case iorio_parse:subscription(Sub) of
                        {ok, Val} -> Val;
                        _ -> invalid
                    end
            end,
    Parsed = lists:map(Parse, Subs),
    OnlyValid = fun (invalid) -> false; (_Val) -> true end,
    lists:filter(OnlyValid, Parsed).


