-module(iorio_rest_stream).

-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         malformed_request/2,
         content_types_accepted/2,
         content_types_provided/2,
         is_authorized/2,
         resource_exists/2,
         delete_resource/2,
         options/2,
         from_json/2,
         from_json_patch/2,
         to_json/2]).

-ignore_xref([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         malformed_request/2,
         content_types_accepted/2,
         content_types_provided/2,
         is_authorized/2,
         resource_exists/2,
         delete_resource/2,
         options/2,
         from_json/2,
         from_json_patch/2,
         to_json/2]).

-record(state, {bucket, stream, from_sn, limit, access, info, filename, creds,
                action, n=3, w=3, timeout=5000, cors, iorio_state, method}).

-include_lib("sblob/include/sblob.hrl").
-include("include/iorio.hrl").

to_int_or(Bin, Default) ->
    Str = binary_to_list(Bin),
    case string:to_integer(Str) of
        {Int, ""} -> Int;
        _ -> Default
    end.

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest};
init({ssl, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{access, Access}, {n, N}, {w, W}, {timeout, Timeout},
                {cors, Cors}, {iorio_state, IorioState}]) ->
    {Bucket, Req1} = cowboy_req:binding(bucket, Req),
    {Stream, Req2} = cowboy_req:binding(stream, Req1),
    {FromSNStr, Req3} = cowboy_req:qs_val(<<"from">>, Req2, <<"">>),
    {LimitStr, Req4} = cowboy_req:qs_val(<<"limit">>, Req3, <<"1">>),
    {Filename, Req5} = cowboy_req:qs_val(<<"download">>, Req4, nil),
    {Method, Req6} = cowboy_req:method(Req5),

    FromSN = to_int_or(FromSNStr, nil),
    Limit = to_int_or(LimitStr, 1),

    Action = get_action(Method, Bucket, Stream),

    {ok, Info} = ioriol_access:new_req([{bucket, Bucket}, {stream, Stream}]),

    {ok, Req6, #state{bucket=Bucket, stream=Stream, from_sn=FromSN,
                      limit=Limit, filename=Filename, n=N, w=W, access=Access,
                      iorio_state=IorioState, info=Info, timeout=Timeout,
                      cors=Cors, method=Method, action=Action}}.

options(Req, State=#state{cors=Cors}) ->
    Req1 = iorio_cors:handle_options(Req, stream, Cors),
    {ok, Req1, State}.

allowed_methods(Req, State) ->
    Methods = [<<"OPTIONS">>, <<"GET">>, <<"POST">>, <<"PATCH">>, <<"DELETE">>],
    {Methods, Req, State}.

malformed_request(Req, State=#state{access=Access, action=Action}) ->
    iorio_http:check_rate_limit(Action, Access, Req, State,
                                fun (St, Creds) -> St#state{creds=Creds} end).

resource_exists(Req, State=#state{method=Method}) ->
    Exists = case Method of
                 <<"POST">> -> false;
                 _ -> true
             end,
    {Exists, Req, State}.

action_for_method(<<"POST">>) ->
    ?PERM_STREAM_PUT;
action_for_method(<<"PATCH">>) ->
    ?PERM_STREAM_PUT;
action_for_method(<<"GET">>) ->
    ?PERM_STREAM_GET;
action_for_method(<<"DELETE">>) ->
    ?PERM_STREAM_DELETE;
action_for_method(<<"OPTIONS">>) ->
    ?PERM_STREAM_GET.

is_authorized(Req, State=#state{access=Access, info=Info, method=Method}) ->
    if Method == <<"OPTIONS">> ->
           {true, Req, State};
       true ->
           Action = action_for_method(Method),
           case iorio_session:fill_session(Req, Access, Info) of
               {ok, Req2, Info1} ->
                   State1 = State#state{info=Info1},
                   case ioriol_access:is_authorized_for_stream(Access, Info1,
                                                               Action) of
                       {ok, Info2} ->
                           {true, Req2, State1#state{info=Info2}};
                       {error, Reason} ->
                           unauthorized_response(Req2, State1, Info1, Reason,
                                                 Action)
                   end;
               {error, Reason, Req1} ->
                   unauthorized_response(Req1, State, Info, Reason, Action)
           end
    end.

content_types_accepted(Req, State=#state{method=Method}) ->
    case Method of
        <<"PATCH">> ->
            {[{{<<"application">>, <<"json-patch+json">>, '*'}, from_json_patch}], Req, State};
        <<"POST">> ->
            {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}
    end.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

sblob_to_json_full(#sblob_entry{seqnum=SeqNum, timestamp=Timestamp,
                                data=Data}) ->
    ["{\"meta\":{\"id\":", integer_to_list(SeqNum), ",\"t\":",
     integer_to_list(Timestamp), "}, \"data\":", Data, "}"].

delete_resource(Req, State=#state{bucket=Bucket, stream=Stream, n=N,
                                  iorio_state=IorioState,
                                  method= <<"DELETE">>, timeout=Timeout}) ->
    lager:info("deleting resource stream ~p/~p", [Bucket, Stream]),
    iorio:delete(IorioState, Bucket, Stream, N, Timeout),
    {true, Req, State}.

to_json(Req, State=#state{bucket=Bucket, stream=Stream, from_sn=From,
                          limit=Limit, filename=Filename,
                          iorio_state=IorioState}) ->
    Blobs = iorio:get(IorioState, Bucket, Stream, From, Limit),
    ItemList = lists:map(fun sblob_to_json_full/1, Blobs),
    ItemsJoined = string:join(ItemList, ","),
    Items = ["[", ItemsJoined, "]"],

    Req1 = if Filename == nil -> Req;
              true ->
                  HeaderVal = io_lib:format("attachment; filename=\"~s\"",
                                            [Filename]),
                  cowboy_req:set_resp_header(<<"Content-Disposition">>,
                                             HeaderVal, Req)
           end,

    {Items, Req1, State}.

put(Bucket, PatchStream, Data,
    #state{n=N, w=W, timeout=Timeout, iorio_state=IorioState}) ->
    iorio:put(IorioState, Bucket, PatchStream, Data, N, W, Timeout).

put_conditionally(Bucket, PatchStream, Data,
                  #state{n=N, w=W, timeout=Timeout, iorio_state=IorioState},
                  LastSeqNum) ->
    iorio:put_conditionally(IorioState, Bucket, PatchStream, Data, LastSeqNum,
                            N, W, Timeout).

publish_patch(Bucket, Stream, Data, State) ->
    PatchStream = list_to_binary(io_lib:format("~s-$patch", [Stream])),
    case put(Bucket, PatchStream, Data, State) of
        {error, Reason}=Error ->
            lager:warning("Error publishing patch ~s:~s ~p",
                          [Bucket, PatchStream, Reason]),
            Error;
        Other -> Other
    end.

store_blob_and_reply(Req, State, Bucket, Stream, Body, WithUriStr) ->
    Result = put(Bucket, Stream, Body, State),
    store_blob_and_reply_1(Req, State, Bucket, Stream, WithUriStr, Result).

store_blob_and_reply(Req, State, Bucket, Stream, Body, WithUriStr,
                     LastSeqNum) ->
    Result = put_conditionally(Bucket, Stream, Body, State, LastSeqNum),
    store_blob_and_reply_1(Req, State, Bucket, Stream, WithUriStr, Result).

store_blob_and_reply_1(Req, State, Bucket, Stream, WithUriStr, PutResult) ->
    case PutResult of
        {ok, SblobEntry} ->
            #sblob_entry{seqnum=SeqNum, timestamp=Timestamp} = SblobEntry,
            Result = ["{\"meta\":{\"id\":", integer_to_list(SeqNum), ",\"t\":",
                      integer_to_list(Timestamp), "}}"],
            Req1 = cowboy_req:set_resp_body(Result, Req),
            SeqNum = SblobEntry#sblob_entry.seqnum,
            UriStr = io_lib:format("/streams/~s/~s/?limit=1&from=~p",
                                   [Bucket, Stream, SeqNum]),
            if WithUriStr -> {{true, UriStr}, Req1, State};
               true -> {true, Req1, State}
            end;
        {error, {conflict, {seqnum, ActualSeqNum, expected, ExpectedSeqNum}}} ->
            Req1 = iorio_http:error(Req, <<"error">>,
                                    [{reason, <<"confict">>},
                                     {field, <<"seqnum">>},
                                     {expected, ExpectedSeqNum},
                                     {got, ActualSeqNum}]),
            {ok, Req2} = cowboy_req:reply(409, Req1),
            {halt, Req2, State};
        {error, Reason} ->
            Req1 = iorio_http:error(Req, <<"error">>, Reason),
            {false, Req1, State}
    end.

read_body(Req, State=#state{bucket=Bucket, stream=Stream}) ->
    case cowboy_req:body(Req) of
        {ok, _Body, _Req1}=Res -> Res;
        {more, _Body, Req1} ->
            lager:warning("request body to large for ~p:~p", [Bucket, Stream]),
            {false, iorio_http:invalid_body(Req1), State};
        {error, Reason} ->
            lager:error("reading request body for ~p:~p: ~p",
                        [Bucket, Stream, Reason]),
            {false, iorio_http:invalid_body(Req), State}
    end.

from_json(Req, State=#state{bucket=Bucket, stream=Stream}) ->
    case read_body(Req, State) of
        {ok, Body, Req1} ->
            case iorio_json:is_json(Body) of
                true ->
                    store_blob_and_reply(Req1, State, Bucket, Stream, Body,
                                         true);
                false ->
                    {false, iorio_http:invalid_body(Req1), State}
            end;
        Other -> Other
    end.

handle_patch(Req, Body,
             State=#state{bucket=Bucket, stream=Stream, iorio_state=IorioState}) ->
    case iorio_json:is_json(Body) of
        true ->
            case jsonpatch:parse(Body) of
                {ok, ParsedPatch} ->
                    case iorio:get_last(IorioState, Bucket, Stream) of
                        {ok, #sblob_entry{data=Data, seqnum=LastSeqNum}} ->
                            ParsedBlob = iorio_json:decode(Data),
                            case jsonpatch:patch(ParsedPatch, ParsedBlob) of
                                {ok, PatchResult} ->
                                    EncodedPatchResult = iorio_json:encode(PatchResult),
                                    Resp = store_blob_and_reply(Req, State,
                                                                Bucket, Stream,
                                                                EncodedPatchResult,
                                                                false, LastSeqNum),
                                    % XXX ugly!
                                    if element(1, Resp) ->
                                           publish_patch(Bucket, Stream, Body, State);
                                       true ->
                                           ok
                                    end,
                                    Resp;
                                _Other ->
                                    % lager doesn't know how to handle maps yet
                                    lager:warning("Error applying patch ~s", [Body]),
                                    {false, iorio_http:invalid_body(Req), State}
                            end;
                        notfound ->
                            {false, iorio_http:error(Req, <<"not-found">>, <<"Not Found">>), State}
                    end;
                {error, Reason} ->
                    % lager doesn't know how to handle maps yet
                    lager:warning("Error parsing patch ~p", [element(1, Reason)]),
                    {false, iorio_http:invalid_body(Req), State}
            end;
        false ->
            {false, iorio_http:invalid_body(Req), State}
    end.

from_json_patch(Req, State) ->
    case read_body(Req, State) of
        {ok, Body, Req1} -> handle_patch(Req1, Body, State);
        Other -> Other
    end.

rest_terminate(_Req, _State) -> ok.

terminate(_Reason, _Req, _State) -> ok.

%% private api

unauthorized_response(Req, State, Info, Reason, Action) ->
    Req1 = iorio_http:no_permission(Req),
    Bucket = ioriol_access:bucket(Info),
    Stream = ioriol_access:stream(Info),
    lager:info("unauthorized stream request on ~p/~p (~p): ~p ~p",
                  [Bucket, Stream, Action, Reason, Info]),
    {{false, <<"jwt">>}, Req1, State}.

get_action(<<"DELETE">>, _Bucket, _Stream) -> delete;
get_action(<<"POST">>, _Bucket, _Stream) -> put;
get_action(<<"PATCH">>, _Bucket, _Stream) -> put_conditionally;
get_action(<<"GET">>, _Bucket, _Stream) -> get;
get_action(<<"OPTIONS">>, _Bucket, _Stream) -> options.
