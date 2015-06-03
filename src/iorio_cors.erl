-module(iorio_cors).
-export([new/1, handle_options/3]).

-define(HEADER_ORIGIN, <<"Access-Control-Allow-Origin">>).
-define(HEADER_HEADERS, <<"Access-Control-Allow-Headers">>).
-define(HEADER_MAX_AGE, <<"Access-Control-Max-Age">>).
-define(HEADER_VARY, <<"Vary">>).

-include("include/iorio.hrl").

new(Opts) ->
    Origins = proplists:get_value(origins, Opts, []),
    Headers = proplists:get_value(headers, Opts, []),
    CsvHeadersStr = string:join(lists:map(fun binary_to_list/1, Headers), ","),
    CsvHeaders = list_to_binary(CsvHeadersStr),
    MaxAge = proplists:get_value(max_age, Opts, 60),
    MaxAgeBin = integer_to_binary(MaxAge),
    Enabled = proplists:get_value(enabled, Opts, false),

    #iorio_cors{origins=Origins,
                headers=Headers,
                csv_headers=CsvHeaders,
                max_age_secs=MaxAgeBin,
                enabled=Enabled}.

handle_options(Req, _EndpointId, #iorio_cors{enabled=false}) ->
    Req;
handle_options(Req, _EndpointId,
               #iorio_cors{csv_headers=Headers, origins=Origins, max_age_secs=MaxAge}) ->
    % TODO: ReqOrigin seems to be undefined, is that ok?
    {ReqOrigin, Req1} = cowboy_req:header(<<"Origin">>, Req),
    OriginInOrigins = Origins == any orelse lists:member(ReqOrigin, Origins),

    if OriginInOrigins ->
           Req2 = cowboy_req:set_resp_header(?HEADER_ORIGIN, <<"*">>, Req1),
           Req3 = cowboy_req:set_resp_header(?HEADER_HEADERS, Headers, Req2),
           Req4 = cowboy_req:set_resp_header(?HEADER_MAX_AGE, MaxAge, Req3),

           % MDN: If the server specifies an origin host rather than "*",
           % then it must also include Origin in the Vary response header to
           % indicate to clients that server responses will differ based on the
           % value of the Origin request header.
           Req5 = cowboy_req:set_resp_header(?HEADER_VARY, <<"Origin">>, Req4),
           Req5;
       true ->
           Req1
    end.
