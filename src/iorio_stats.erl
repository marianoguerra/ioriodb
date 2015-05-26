-module(iorio_stats).
-export([all_stats/0, cowboy_response_hook/4, init_metrics/0]).

-behaviour(cowboy_middleware).
-export([execute/2]).

-define(METRIC_HTTP_ACTIVE_REQS, [iorio, api, http, active_requests]).
-define(METRIC_HTTP_REQ_TIME, [iorio, api, http, req_time]).

-define(ENDPOINTS, [<<"listen">>, <<"streams">>, <<"buckets">>, <<"access">>,
                    <<"sessions">>, <<"users">>, <<"stats">>, <<"ping">>,
                    <<"x">>, <<"ui">>]).

-define(STATUS_CODES, [200, 201, 202, 203, 204, 205, 206,
                       300, 301, 302, 303, 304, 305, 306, 307, 308,

                       400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410,
                       411, 412, 413, 414, 415, 416, 417, 418, 419, 421, 426,
                       428, 429, 431,

                       500, 501, 502, 503, 504, 505, 506, 511]).

all_stats() ->
 [{node, node_stats()},
  {riak_core, riak_core_stats()},
  {file, file_stats()},
  {http, http_stats()}].

node_stats() ->
 [{Abs1, Inc1}] = recon:node_stats_list(1, 0),
 [{abs, Abs1}, {inc, Inc1}].

file_stats() ->
    file_handle_cache:info().

riak_core_stats() ->
    Stats = riak_core_stat:get_stats(),
    KeyToString = fun ({K, V}) ->
                          StrKeyTokens = lists:map(fun to_string/1, K),
                          StrKey = string:join(StrKeyTokens, "."),
                          BinKey = list_to_binary(StrKey),
                          {BinKey, V}
                  end,
    lists:map(KeyToString, Stats).

endpoint_key(Type, EndPoint) ->
    [iorio, api, http, Type, EndPoint].

get_endpoint_min_value(EndPoint) ->
    get_endpoint_value(req_min, EndPoint).

get_endpoint_time_value(EndPoint) ->
    get_endpoint_value(req_time, EndPoint).

resp_code_key(Code) ->
    [iorio, api, http, resp, Code].

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

create_endpoint_min_metric(EndPoint) ->
    exometer:new(endpoint_key(req_min, EndPoint), spiral, [{time_span, 60000}]).

create_endpoint_time_metric(EndPoint) ->
    exometer:new(endpoint_key(req_time, EndPoint), histogram, []).

create_resp_code_metric(Code) ->
    exometer:new(resp_code_key(Code), spiral, [{time_span, 60000}]).

http_stats() ->
    [{active_reqs, unwrap_metric_value(?METRIC_HTTP_ACTIVE_REQS)},
     {resp_code, lists:map(fun get_resp_code_value/1, ?STATUS_CODES)},
     {req_time, lists:map(fun get_endpoint_time_value/1, ?ENDPOINTS)},
     {req_min, lists:map(fun get_endpoint_min_value/1, ?ENDPOINTS)}].

to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(V) when is_integer(V) -> integer_to_list(V).

init_metrics() ->
    lists:map(fun create_endpoint_time_metric/1, ?ENDPOINTS),
    lists:map(fun create_endpoint_min_metric/1, ?ENDPOINTS),
    lists:map(fun create_resp_code_metric/1, ?STATUS_CODES),

    exometer:new(?METRIC_HTTP_ACTIVE_REQS, counter, []).

cowboy_response_hook(Code, _Headers, _Body, Req) ->
    EndTs = now_fast(),

    {Path, _Req1} = cowboy_req:path(Req),
    [<<>>, EndPoint|_] = binary:split(Path, <<"/">>, [global]),
    {_Method, _Req2} = cowboy_req:method(Req),
    {StartTs, _Req3} = cowboy_req:meta(iorio_req_start, Req),

    ReqTime = EndTs - StartTs,

    exometer:update(endpoint_key(req_time, EndPoint), ReqTime),
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
