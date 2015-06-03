-module(ioriox_echo).
-export([init_req/2, allowed_methods/2, is_authorized/2, handle/5, handle/6,
         stop_req/2, terminate_req/2]).

-ignore_xref([init_req/2, allowed_methods/2, is_authorized/2, handle/5,
              handle/6, stop/2, terminate/2]).

-record(state, {access}).

init_req(Req, Access) ->
    {#state{access=Access}, Req}.

allowed_methods(_State, Req) ->
    {[<<"GET">>, <<"DELETE">>, <<"PUT">>, <<"POST">>], Req}.

is_authorized(_State, Req) -> {true, Req}.

handle(_State, Req, Method, PathInfo, Query) ->
    Response = [{method, Method}, {path, PathInfo}, {query, Query}],
    {ok, {json, Response}, Req}.

handle(_State, Req, Method, PathInfo, Query, Body) ->
    Response = [{method, Method}, {path, PathInfo}, {query, Query}, {body, Body}],
    {ok, {json, Response}, Req}.

stop_req(_State, _Req) -> ok.
terminate_req(_State, _Req) -> ok.
