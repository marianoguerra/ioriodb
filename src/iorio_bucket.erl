-module(iorio_bucket).
-export([base_path/0, is_empty/1, delete/1, foldl_gblobs/3,
         list_streams/2, list_buckets/1, have_bucket/2]).

base_path() ->
    CurrentDir = filename:absname("."),
    DefaultDataDir = filename:join([CurrentDir, "iorio_data"]),
    application:get_env(iorio, data_path, DefaultDataDir).

is_empty(Path) ->
    (not filelib:is_dir(Path)) orelse (length(list_buckets(Path)) == 0).

delete(Path) ->
    sblob_util:remove(Path).

foldl_gblobs(Path, Fun, Acc0) ->
    GblobNames = list_gblob_names(Path),
    lists:foldl(fun ({BucketName, GblobName}, AccIn) ->
                        BucketPath = filename:join([Path, BucketName, GblobName]),
                        Gblob = gblob:open(BucketPath, []),
                        AccOut = Fun({BucketName, Gblob}, AccIn),
                        gblob:close(Gblob),

                        AccOut
                end, Acc0, GblobNames).

list_gblobs_for_bucket(Path, BucketName) ->
    BucketPath = filename:join([Path, BucketName]),
    list_dir(BucketPath).

list_streams(Path, Bucket) ->
    Streams = list_gblobs_for_bucket(Path, Bucket),
    StreamsBin = lists:map(fun list_to_binary/1, Streams),
    StreamsBin.

list_buckets(Path) ->
    lists:map(fun list_to_binary/1, list_dir(Path)).

list_dir(Path) ->
    case file:list_dir(Path) of
        {error, enoent} -> [];
        {ok, Names} -> Names
    end.

have_bucket(Path, Bucket) ->
    BucketPath = filename:join([Path, Bucket]),
    filelib:is_dir(BucketPath).

list_gblob_names(Path) ->
    BucketNames = list_buckets(Path),
    lists:foldl(fun (BucketName, AccIn) ->
                        BucketPath = filename:join([Path, BucketName]),
                        GblobNames = list_dir(BucketPath),
                        lists:foldl(fun (StreamName, Items) ->
                                            StreamNameBin = list_to_binary(StreamName),
                                            [{BucketName, StreamNameBin}|Items]
                                    end, AccIn, GblobNames)
                end, [], BucketNames).

