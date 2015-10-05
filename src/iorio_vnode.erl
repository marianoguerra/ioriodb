-module(iorio_vnode).
-behaviour(riak_core_vnode).

-include("iorio.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").
-include_lib("sblob/include/sblob.hrl").
-include_lib("sblob/include/gblob.hrl").
-include("include/iorio_node_state.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         handle_info/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-ignore_xref([start_vnode/1, handle_info/2]).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    iorio_node:init([{partition, Partition}]).

handle_command(ping, _Sender, State) ->
    iorio_node:ping(State);

handle_command({put, ReqId, BucketName, Stream, Data}, _Sender, State) ->
    iorio_node:put(State, ReqId, BucketName, Stream, Data);

handle_command({coord_put_conditionally, N, W, Bucket, Stream, Data, LastSeqNum, Pid},
               _Sender, State) ->
    iorio_node:coord_put_conditionally(State, N, W, Bucket, Stream, Data,
                                       LastSeqNum, Pid);

handle_command({coord_put, N, W, Bucket, Stream, Data, Pid}, _Sender, State) ->
    iorio_node:coord_put(State, N, W, Bucket, Stream, Data, Pid);

handle_command({put_conditionally, ReqId, BucketName, Stream, Data, LastSeqNum},
               _Sender, State) ->
    iorio_node:put_conditionally(State, ReqId, BucketName, Stream, Data,
                                 LastSeqNum);

handle_command({get, BucketName, Stream, From, Count}, Sender, State) ->
    Callback = fun (Entries) -> riak_core_vnode:reply(Sender, Entries) end,
    iorio_node:get(State, BucketName, Stream, From, Count, Callback);

handle_command({delete, ReqId, BucketName, Stream}, _Sender, State) ->
    iorio_node:delete(State, ReqId, BucketName, Stream);

handle_command({subscribe, BucketName, Stream, FromSeqNum, Pid}, _Sender, State) ->
    iorio_node:subscribe(State, BucketName, Stream, FromSeqNum, Pid);

handle_command({unsubscribe, BucketName, Stream, Pid}, _Sender, State) ->
    iorio_node:unsubscribe(State, BucketName, Stream, Pid);

handle_command(Message, _Sender, State) ->
    lager:warning("unknown command ~p", [Message]),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=FoldFun, acc0=Acc0}, _Sender,
                       State=#state{partition=Partition, path=Path}) ->
    lager:info("fold req ~p", [Partition]),
    Opts = [],
    GblobFoldFun = fun ({BucketName, StreamName,
                         #sblob_entry{seqnum=SeqNum, timestamp=Ts, data=Data}}, AccEntry) ->
                           Key = {BucketName, StreamName},
                           Val = {SeqNum, Ts, Data},
                           AccEntryOut = FoldFun(Key, Val, AccEntry),
                           {continue, AccEntryOut}
                   end,

    Fun = fun({BucketName, Gblob=#gblob{name=StreamName}}, AccL) ->
                  lager:info("fold gblob ~s ~s", [BucketName, StreamName]),
                  FunI = fun (Entry, AccInner) ->
                                 EntryData = {BucketName, StreamName, Entry},
                                 GblobFoldFun(EntryData, AccInner)
                         end,
                  case gblob_util:fold(Gblob, Opts, FunI, AccL) of
                      {error, Reason, AccLOut} ->
                          lager:warning("Error while folding gblob ~p",
                                        [Reason]),
                          AccLOut;
                      {_StopReason, AccLOut} -> AccLOut
                  end
          end,
    Acc = iorio_bucket:foldl_gblobs(Path, Fun, Acc0),
    {reply, Acc, State};

handle_handoff_command(Message, Sender, State=#state{partition=Partition}) ->
    lager:warning("handling command during handoff, state may diverge ~p",
                  [Partition]),
    handle_command(Message, Sender, State).

handoff_starting(_TargetNode, State=#state{partition=Partition}) ->
    lager:info("handoff starting ~p", [Partition]),
    {true, State}.

handoff_cancelled(State=#state{partition=Partition}) ->
    lager:info("handoff cancelled ~p", [Partition]),
    {ok, State}.

handoff_finished(_TargetNode, State=#state{partition=Partition}) ->
    lager:info("handoff finished ~p", [Partition]),
    {ok, State}.

handle_handoff_data(BinData, State=#state{buckets=Buckets}) ->
    TermData = binary_to_term(BinData),
    lager:debug("handoff data received ~p", [TermData]),
    {{BucketName, StreamName}, {SeqNum, Ts, Data}} = TermData,
    Entry = iorio_vnode_buckets:raw_put(Buckets, BucketName, StreamName, Ts, Data),
    GotSeqNum = Entry#sblob_entry.seqnum,
    if SeqNum =/= GotSeqNum ->
           lager:warning("seqnum mismatch on entry handoff expected ~p but got ~p",
                         [SeqNum, GotSeqNum]);
       true -> ok
    end,
    {reply, ok, State}.

encode_handoff_item(Key, Value) ->
    term_to_binary({Key, Value}).

is_empty(State=#state{path=Path, partition=Partition}) ->
    IsEmpty = iorio_bucket:is_empty(Path),
    lager:info("handoff is empty? ~p ~p", [IsEmpty, Partition]),
    {IsEmpty, State}.

delete(State=#state{partition=Partition}) ->
    lager:info("handoff delete ~p", [Partition]),
    iorio_node:delete(State).

handle_coverage({list_streams, Bucket}, _KeySpaces, {_, RefId, _}, State) ->
    iorio_node:list_streams(State, Bucket, RefId);

handle_coverage({list_buckets}, _KeySpaces, {_, RefId, _}, State) ->
    iorio_node:list_buckets(State, RefId);

handle_coverage({size, BucketName}, _KeySpaces, {_, RefId, _}, State) ->
    iorio_node:bucket_size(State, BucketName, RefId);

handle_coverage({truncate_percentage, BucketName, Percentage}, _KeySpaces,
                {_, RefId, _}, State) ->
    iorio_node:truncate_percentage(State, BucketName, Percentage, RefId);

handle_coverage(Req, _KeySpaces, _Sender, State) ->
    lager:warning("unknown coverage received ~p", [Req]),
    {noreply, State}.

handle_exit(_Pid, _Reason, State=#state{partition=Partition}) ->
    lager:info("vnode: handle exit ~p", [Partition]),
    iorio_node:free_resources(State),
    {noreply, State}.

handle_info(evict_bucket, State) ->
    iorio_node:evict_bucket(State);

handle_info(Msg, State=#state{partition=Partition}) ->
    lager:warning("vnode: Unexpected handle info msg: ~p ~p", [Partition, Msg]),
    {ok, State}.

terminate(Reason, State=#state{partition=Partition}) ->
    lager:info("terminate ~p ~p", [Partition, Reason]),
    iorio_node:free_resources(State),
    ok.

