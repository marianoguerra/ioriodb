-module(iorio_stats).
-export([all_stats/2, cowboy_response_hook/4, init_metrics/0]).
-export([start_metric_sender/5, send_metrics/4,
         bucket_active/1, bucket_inactive/1, bucket_last_check/1,
         bucket_per_vnode/1, bucket_last_action/1, bucket_last_eviction/1,
         bucket_no_action/1, bucket_no_check/1, bucket_no_eviction/1]).

-export([auth_error/0, auth_success/0, listen_connect/1, listen_disconnect/1]).

-export([core_ping/0, core_put/0, core_get/0, core_delete/0,
         core_subscribe/0, core_unsubscribe/0,
         core_list_buckets/0, core_list_streams/0,
         core_truncate/0,
         core_msg_size/1,
         channel_create/0, channel_destroy/0,
         channel_size/1, channel_size_bytes/1,
         channel_count/1, channel_sub_count/1,
         log_level/1]).

-behaviour(cowboy_middleware).
-export([execute/2]).

-ignore_xref([all_stats/2, send_metrics/2]).

-define(METRIC_AUTH_ERROR, [iorio, auth, error]).
-define(METRIC_AUTH_SUCCESS, [iorio, auth, success]).
-define(METRIC_LISTEN_ONCE, [iorio, listen, active, once]).
-define(METRIC_LISTEN_ACTIVE, [iorio, listen, active, active]).
-define(METRIC_LISTEN_FALSE, [iorio, listen, active, not_active]).
-define(METRIC_HTTP_ACTIVE_REQS, [iorio, api, http, active_requests]).
-define(METRIC_HTTP_REQ_TIME, [iorio, api, http, req_time]).

-define(METRIC_CORE_PING, [iorio, core, ping]).
-define(METRIC_CORE_PUT, [iorio, core, put]).
-define(METRIC_CORE_GET, [iorio, core, get]).
-define(METRIC_CORE_DELETE, [iorio, core, delete]).
-define(METRIC_CORE_SUBSCRIBE, [iorio, core, subscribe]).
-define(METRIC_CORE_UNSUBSCRIBE, [iorio, core, unsubscribe]).
-define(METRIC_CORE_LIST_BUCKETS, [iorio, core, list, buckets]).
-define(METRIC_CORE_LIST_STREAMS, [iorio, core, list, streams]).
-define(METRIC_CORE_TRUNCATE, [iorio, core, truncate]).

-define(METRIC_CHANNEL_CREATE, [iorio, channels, create]).
-define(METRIC_CHANNEL_DESTROY, [iorio, channels, destroy]).
-define(METRIC_CHANNEL_RUNNING, [iorio, channels, running]).

-define(METRIC_CHANNEL_SIZE, [iorio, channels, size]).
-define(METRIC_CHANNEL_SIZE_BYTES, [iorio, channels, size_bytes]).
-define(METRIC_CHANNEL_SUB_COUNT, [iorio, channels, sub_count]).
-define(METRIC_CHANNEL_COUNT, [iorio, channels, count]).

-define(METRIC_BUCKET_ACTIVE, [iorio, buckets, active]).
-define(METRIC_BUCKET_INACTIVE, [iorio, buckets, inactive]).
-define(METRIC_BUCKET_PER_VNODE, [iorio, buckets, per_vnode]).
-define(METRIC_BUCKET_LAST_CHECK, [iorio, buckets, last_check]).
-define(METRIC_BUCKET_LAST_ACTION, [iorio, buckets, last_action]).
-define(METRIC_BUCKET_LAST_EVICTION, [iorio, buckets, last_eviction]).
-define(METRIC_BUCKET_NO_ACTION, [iorio, buckets, no_action]).
-define(METRIC_BUCKET_NO_CHECK, [iorio, buckets, no_check]).
-define(METRIC_BUCKET_NO_EVICTION, [iorio, buckets, no_eviction]).

-define(METRIC_CORE_MSG_SIZE, [iorio, core, msg, size]).

-define(ENDPOINTS, [<<"listen">>, <<"streams">>, <<"buckets">>, <<"access">>,
                    <<"sessions">>, <<"users">>, <<"stats">>, <<"ping">>,
                    <<"x">>, <<"ui">>]).

-define(STATUS_CODES, [200, 201, 202, 203, 204, 205, 206,
                       300, 301, 302, 303, 304, 305, 306, 307, 308,

                       400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410,
                       411, 412, 413, 414, 415, 416, 417, 418, 419, 421, 426,
                       428, 429, 431,

                       500, 501, 502, 503, 504, 505, 506, 511]).

all_stats(Iorio, IorioState) ->
 [{node, node_stats()},
  {iorio, Iorio:stats(IorioState)},
  {file, file_stats()},
  {log, log_stats()},
  {http, http_stats()},
  {channel, channel_stats()},
  {bucket, bucket_stats()},
  {core, core_stats()}].

node_stats() ->
 [{Abs1, Inc1}] = recon:node_stats_list(1, 0),
 [{abs, Abs1}, {inc, Inc1}].

file_stats() ->
    file_handle_cache:info().

auth_error()    -> exometer:update(?METRIC_AUTH_ERROR, 1).
auth_success() -> exometer:update(?METRIC_AUTH_SUCCESS, 1).

listen_connect(once)  -> exometer:update(?METRIC_LISTEN_ONCE, 1);
listen_connect(true)  -> exometer:update(?METRIC_LISTEN_ACTIVE, 1);
listen_connect(false) -> exometer:update(?METRIC_LISTEN_FALSE, 1).

listen_disconnect(once) -> exometer:update(?METRIC_LISTEN_ONCE, -1);
listen_disconnect(true) -> exometer:update(?METRIC_LISTEN_ACTIVE, -1);
listen_disconnect(false) -> exometer:update(?METRIC_LISTEN_FALSE, -1).

core_ping()         -> exometer:update(?METRIC_CORE_PING, 1).
core_put()          -> exometer:update(?METRIC_CORE_PUT, 1).
core_get()          -> exometer:update(?METRIC_CORE_GET, 1).
core_delete()       -> exometer:update(?METRIC_CORE_DELETE, 1).
core_subscribe()    -> exometer:update(?METRIC_CORE_SUBSCRIBE, 1).
core_unsubscribe()  -> exometer:update(?METRIC_CORE_UNSUBSCRIBE, 1).
core_list_buckets() -> exometer:update(?METRIC_CORE_LIST_BUCKETS, 1).
core_list_streams() -> exometer:update(?METRIC_CORE_LIST_STREAMS, 1).
core_truncate()     -> exometer:update(?METRIC_CORE_TRUNCATE, 1).

channel_create()  ->
    exometer:update(?METRIC_CHANNEL_CREATE, 1),
    exometer:update(?METRIC_CHANNEL_RUNNING, 1).
channel_destroy() ->
    exometer:update(?METRIC_CHANNEL_DESTROY, 1),
    exometer:update(?METRIC_CHANNEL_RUNNING, -1).

channel_size(Size) -> exometer:update(?METRIC_CHANNEL_SIZE, Size).
channel_size_bytes(Size) -> exometer:update(?METRIC_CHANNEL_SIZE_BYTES, Size).
channel_sub_count(Count) -> exometer:update(?METRIC_CHANNEL_SUB_COUNT, Count).
channel_count(Count) -> exometer:update(?METRIC_CHANNEL_COUNT, Count).

bucket_active(Value) ->
    exometer:update(?METRIC_BUCKET_ACTIVE, Value).
bucket_inactive(Value) ->
    exometer:update(?METRIC_BUCKET_INACTIVE, Value).
bucket_per_vnode(Value) ->
    exometer:update(?METRIC_BUCKET_PER_VNODE, Value).
bucket_last_check(Value) ->
    exometer:update(?METRIC_BUCKET_LAST_CHECK, Value).
bucket_last_action(Value) ->
    exometer:update(?METRIC_BUCKET_LAST_ACTION, Value).
bucket_last_eviction(Value) ->
    exometer:update(?METRIC_BUCKET_LAST_EVICTION, Value).
bucket_no_action(Value) ->
    exometer:update(?METRIC_BUCKET_NO_ACTION, Value).
bucket_no_check(Value) ->
    exometer:update(?METRIC_BUCKET_NO_CHECK, Value).
bucket_no_eviction(Value) ->
    exometer:update(?METRIC_BUCKET_NO_EVICTION, Value).

core_msg_size(Size) -> exometer:update(?METRIC_CORE_MSG_SIZE, Size).

log_level(Level) -> lager_exometer_backend_metrics:log_level(Level).

endpoint_key(Type, EndPoint) ->
    [iorio, api, http, Type, EndPoint].

get_endpoint_min_value(EndPoint) ->
    get_endpoint_value(req_min, EndPoint).

get_endpoint_time_value(EndPoint) ->
    get_endpoint_value(req_time, EndPoint).

resp_code_key(Code) -> [iorio, api, http, resp, Code].

unwrap_metric_value(Key) ->
    case exometer:get_value(Key) of
        {ok, Val} -> Val;
        Other -> 
            lager:warning("Error getting endpoint value ~p: ~p", [Key, Other]),
            []
    end.

get_endpoint_value(Type, EndPoint) ->
    Value = unwrap_metric_value(endpoint_key(Type, EndPoint)),
    {EndPoint, Value}.

get_resp_code_value(Code) ->
    Value = unwrap_metric_value(resp_code_key(Code)),
    {Code, Value}.

get_log_level_value(Level) ->
    Value = unwrap_metric_value(lager_exometer_backend_metrics:log_level_key(Level)),
    {Level, Value}.

create_endpoint_min_metric(EndPoint) ->
    exometer:new(endpoint_key(req_min, EndPoint), spiral, [{time_span, 60000}]).

create_endpoint_time_metric(EndPoint) ->
    exometer:new(endpoint_key(req_time, EndPoint), histogram, []).

create_resp_code_metric(Code) ->
    exometer:new(resp_code_key(Code), spiral, [{time_span, 60000}]).

http_stats() ->
    [{listen, [{active, unwrap_metric_value(?METRIC_LISTEN_ACTIVE)},
               {not_active, unwrap_metric_value(?METRIC_LISTEN_FALSE)},
               {once, unwrap_metric_value(?METRIC_LISTEN_ONCE)}]},
     {auth, [{error, unwrap_metric_value(?METRIC_AUTH_ERROR)},
             {success, unwrap_metric_value(?METRIC_AUTH_SUCCESS)}]},
     {resp, [{by_code, lists:map(fun get_resp_code_value/1, ?STATUS_CODES)}]},
     {req, [{time, lists:map(fun get_endpoint_time_value/1, ?ENDPOINTS)},
            {active, unwrap_metric_value(?METRIC_HTTP_ACTIVE_REQS)},
            {count, lists:map(fun get_endpoint_min_value/1, ?ENDPOINTS)}]}].

log_stats() ->
     [{by_level, lists:map(fun get_log_level_value/1,
                           lager_exometer_backend_metrics:log_levels())}].

core_stats() ->
    [{ping, unwrap_metric_value(?METRIC_CORE_PING)},
     {put, unwrap_metric_value(?METRIC_CORE_PUT)},
     {get, unwrap_metric_value(?METRIC_CORE_GET)},
     {delete, unwrap_metric_value(?METRIC_CORE_DELETE)},
     {subscribe, unwrap_metric_value(?METRIC_CORE_SUBSCRIBE)},
     {unsubscribe, unwrap_metric_value(?METRIC_CORE_UNSUBSCRIBE)},
     {list_buckets, unwrap_metric_value(?METRIC_CORE_LIST_BUCKETS)},
     {list_streams, unwrap_metric_value(?METRIC_CORE_LIST_STREAMS)},
     {truncate, unwrap_metric_value(?METRIC_CORE_TRUNCATE)},
     {msg_size, unwrap_metric_value(?METRIC_CORE_MSG_SIZE)}].

channel_stats() ->
     [{create, unwrap_metric_value(?METRIC_CHANNEL_CREATE)},
      {destroy, unwrap_metric_value(?METRIC_CHANNEL_DESTROY)},
      {running, unwrap_metric_value(?METRIC_CHANNEL_RUNNING)},
      {size, unwrap_metric_value(?METRIC_CHANNEL_SIZE)},
      {size_bytes, unwrap_metric_value(?METRIC_CHANNEL_SIZE_BYTES)},
      {sub_count, unwrap_metric_value(?METRIC_CHANNEL_SUB_COUNT)},
      {count, unwrap_metric_value(?METRIC_CHANNEL_COUNT)}].

bucket_stats() ->
    [{active, unwrap_metric_value(?METRIC_BUCKET_ACTIVE)},
     {inactive, unwrap_metric_value(?METRIC_BUCKET_INACTIVE)},
     {per_vnode, unwrap_metric_value(?METRIC_BUCKET_PER_VNODE)},
     {no_action, unwrap_metric_value(?METRIC_BUCKET_NO_ACTION)},
     {no_check, unwrap_metric_value(?METRIC_BUCKET_NO_CHECK)},
     {no_eviction, unwrap_metric_value(?METRIC_BUCKET_NO_EVICTION)},
     {last_check, unwrap_metric_value(?METRIC_BUCKET_LAST_CHECK)},
     {last_action, unwrap_metric_value(?METRIC_BUCKET_LAST_ACTION)},
     {last_eviction, unwrap_metric_value(?METRIC_BUCKET_LAST_EVICTION)}].

init_metrics() ->
    lists:map(fun create_endpoint_time_metric/1, ?ENDPOINTS),
    lists:map(fun create_endpoint_min_metric/1, ?ENDPOINTS),
    lists:map(fun create_resp_code_metric/1, ?STATUS_CODES),

    exometer:new(?METRIC_AUTH_ERROR, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_AUTH_SUCCESS, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_HTTP_ACTIVE_REQS, counter, []),

    exometer:new(?METRIC_LISTEN_ONCE, counter, []),
    exometer:new(?METRIC_LISTEN_ACTIVE, counter, []),
    exometer:new(?METRIC_LISTEN_FALSE, counter, []),

    exometer:new(?METRIC_CHANNEL_RUNNING, counter, []),
    exometer:new(?METRIC_CHANNEL_CREATE, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_CHANNEL_DESTROY, spiral, [{time_span, 60000}]),

    exometer:new(?METRIC_CHANNEL_SIZE, histogram, []),
    exometer:new(?METRIC_CHANNEL_SIZE_BYTES, histogram, []),
    exometer:new(?METRIC_CHANNEL_SUB_COUNT, histogram, []),
    exometer:new(?METRIC_CHANNEL_COUNT, spiral, [{time_span, 60000}]),

    exometer:new(?METRIC_BUCKET_ACTIVE, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_BUCKET_INACTIVE, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_BUCKET_NO_ACTION, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_BUCKET_NO_CHECK, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_BUCKET_NO_EVICTION, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_BUCKET_PER_VNODE, histogram, []),
    exometer:new(?METRIC_BUCKET_LAST_CHECK, histogram, []),
    exometer:new(?METRIC_BUCKET_LAST_ACTION, histogram, []),
    exometer:new(?METRIC_BUCKET_LAST_EVICTION, histogram, []),

    exometer:new(?METRIC_CORE_PING, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_CORE_PUT, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_CORE_GET, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_CORE_DELETE, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_CORE_SUBSCRIBE, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_CORE_UNSUBSCRIBE, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_CORE_LIST_BUCKETS, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_CORE_LIST_STREAMS, spiral, [{time_span, 60000}]),
    exometer:new(?METRIC_CORE_TRUNCATE, spiral, [{time_span, 60000}]),

    exometer:new(?METRIC_CORE_MSG_SIZE, histogram, []).

send_metrics(Iorio, IorioState, Bucket, Stream) ->
    Metrics = all_stats(Iorio, IorioState),
    Node = erlang:node(),
    FullMetrics = [{node, Node}, {metrics, Metrics}],
    FullMetricsJson = iorio_json:encode(FullMetrics),
    case Iorio:put(IorioState, Bucket, Stream, FullMetricsJson) of
        {ok, _Entry} -> ok;
        Other ->
            lager:error("Sending Metrics: ~p", [Other])
    end.

start_metric_sender(IorioMod, IorioState, Bucket, Stream, IntervalMs) ->
    timer:apply_interval(IntervalMs, ?MODULE, send_metrics,
                         [IorioMod, IorioState, Bucket, Stream]).

cowboy_response_hook(Code, _Headers, _Body, Req) ->
    EndTs = now_fast(),

    {Path, _Req1} = cowboy_req:path(Req),
    EndPoint = case binary:split(Path, <<"/">>, [global]) of
                   [<<>>, EndPoint0|_] -> EndPoint0;
                   [<<>>] -> <<"">>;
                   [EndPoint0] -> EndPoint0
               end,
    {_Method, _Req2} = cowboy_req:method(Req),
    {StartTs, _Req3} = cowboy_req:meta(iorio_req_start, Req),

    if is_integer(StartTs) ->
        ReqTime = EndTs - StartTs,
        exometer:update(endpoint_key(req_time, EndPoint), ReqTime);
       true -> ok
    end,

    exometer:update(endpoint_key(req_min, EndPoint), 1),
    exometer:update(resp_code_key(Code), 1),
    exometer:update(?METRIC_HTTP_ACTIVE_REQS, -1),

    Req.

now_fast() ->
    {Mega, Sec, Micro} = os:timestamp(),
    ((Mega * 1000000 + Sec) * 1000000 + Micro).

execute(Req, Env) ->
    Now = now_fast(),
    Req1 = cowboy_req:set_meta(iorio_req_start, Now, Req),

    exometer:update(?METRIC_HTTP_ACTIVE_REQS, 1),

    {ok, Req1, Env}.
