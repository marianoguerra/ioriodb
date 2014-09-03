-module(iorio_vnode).
-behaviour(riak_core_vnode).
-include("iorio.hrl").

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
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-ignore_xref([
             start_vnode/1
             ]).

-record(state, {partition, path, buckets, channels}).

get_bucket(State=#state{buckets=Buckets, path=Path}, BucketName) ->
    case sblob_preg:get(Buckets, BucketName) of
        none -> 
            BucketNameStr = binary_to_list(BucketName),
            BucketPath = filename:join([Path, BucketNameStr]),
            {ok, Bucket} = gblob_bucket:start(BucketPath, [], []),
            NewBuckets = sblob_preg:put(Buckets, BucketName, Bucket),
            NewState = State#state{buckets=NewBuckets},

            {NewState, Bucket};
        {value, Bucket} ->
            {State, Bucket}
    end.

get_channel(State=#state{channels=Channels}, BucketName, Key) ->
    ChannelKey = {BucketName, Key},
    case sblob_preg:get(Channels, ChannelKey) of
        none -> 
            {ok, Channel} = iorio_channel:new(),
            NewChannels= sblob_preg:put(Channels, ChannelKey, Channel),
            NewState = State#state{channels=NewChannels},

            {NewState, Channel};
        {value, Channel} ->
            {State, Channel}
    end.

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    CurrentDir = filename:absname("."),
    DefaultDataDir = filename:join([CurrentDir, "iorio_data"]),
    BasePath = application:get_env(iorio, data_path, DefaultDataDir),

    PartitionStr = integer_to_list(Partition),
    Path = filename:join([BasePath, PartitionStr]),
    Buckets = sblob_preg:new(),
    Channels = sblob_preg:new(),

    {ok, #state{partition=Partition, path=Path, buckets=Buckets, channels=Channels}}.

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};

handle_command({put, BucketName, Stream, Data}, _Sender,
               State=#state{partition=Partition}) ->
    lager:debug("put ~s ~s at ~p", [BucketName, Stream, Partition]),
    {State1, Channel} = get_channel(State, BucketName, Stream),
    {NewState, Bucket} = get_bucket(State1, BucketName),
    Entry = gblob_bucket:put(Bucket, Stream, Data),
    iorio_channel:send(Channel, {entry, BucketName, Stream, Entry}),
    {reply, Entry, NewState};

handle_command({get, BucketName, Stream, From, Count}, _Sender,
               State=#state{partition=Partition}) ->
    lager:debug("get ~s ~s ~p ~p at ~p", [BucketName, Stream, From, Count, Partition]),
    {NewState, Bucket} = get_bucket(State, BucketName),
    Entries = gblob_bucket:get(Bucket, Stream, From, Count),
    {reply, Entries, NewState};

handle_command({subscribe, BucketName, Stream, Pid}, _Sender, State=#state{partition=Partition}) ->
    lager:debug("subscribe ~s ~s at ~p", [BucketName, Stream, Partition]),
    {NewState, Channel} = get_channel(State, BucketName, Stream),
    % TODO do something with return value?
    iorio_channel:subscribe(Channel, Pid),
    {reply, ok, NewState};

handle_command({unsubscribe, BucketName, Stream, Pid}, _Sender, State=#state{partition=Partition}) ->
    lager:debug("unsubscribe ~s ~s at ~p", [BucketName, Stream, Partition]),
    % TODO: don't create it if it doesn't exist
    {NewState, Channel} = get_channel(State, BucketName, Stream),
    % TODO do something with return value?
    iorio_channel:unsubscribe(Channel, Pid),
    {reply, ok, NewState};

handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    State1 = free_resources(State),
    {noreply, State1}.

terminate(_Reason, State) ->
    _State1 = free_resources(State),
    ok.

%% private api

free_resources(State=#state{buckets=Buckets}) ->
    EmptyBuckets = sblob_preg:new(),
    sblob_preg:foreach(Buckets, fun (_Key, Bucket) -> gblob:close(Bucket) end),
    State1 = State#state{buckets=EmptyBuckets},
    State1.
