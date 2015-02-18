-module(iorio_access_handler).

-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         is_authorized/2,
         to_json/2,
         from_json/2]).

-ignore_xref([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         is_authorized/2,
         to_json/2,
         from_json/2]).

-record(state, {access, info}).

-define(FIELD_PERMISSION, <<"permission">>).
-define(FIELD_ROLE, <<"role">>).
-define(FIELD_ACTION, <<"action">>).

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest};
init({ssl, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{access, Access}]) ->
    {Bucket, Req1} = cowboy_req:binding(bucket, Req),
    {Stream, Req2} = cowboy_req:binding(stream, Req1, any),
    {ok, Info} = ioriol_access:new_req([{bucket, Bucket}, {stream, Stream}]),
	{ok, Req2, #state{access=Access, info=Info}}.

allowed_methods(Req, State) -> {[<<"POST">>, <<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

is_authorized(Req, State=#state{access=Access}) ->
    {ok, Req1, State1=#state{info=Info1}} = fill_session(Req, State),
    case ioriol_access:is_authorized(Access, Info1) of
        {ok, Info2} ->
            {true, Req1, State1#state{info=Info2}};
        {error, Reason} ->
            Username = ioriol_access:username(Info1),
            lager:warning("unauthorized access operation user ~p reason ~p",
                          [Username, Reason]),
            Req2 = iorio_http:no_permission(Req1),
            {{false, <<"jwt">>}, Req2, State1}
    end.

from_json(Req, State=#state{info=Info}) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    Info = iorio_json:decode_plist(Body),
    Action = proplists:get_value(?FIELD_ACTION, Info),
    Permission = proplists:get_value(?FIELD_PERMISSION, Info),
    Role = proplists:get_value(?FIELD_ROLE, Info),

    handle_update(Req1, Info, State, Action, Role, Permission).

to_json(Req, State=#state{access=Access, info=Info}) ->
    {ok, AccessPList} = ioriol_access:access_details(Access, Info),
    AccessJsonStr = iorio_json:encode(AccessPList),
    {AccessJsonStr, Req, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.

% private api

fill_session(Req, State=#state{access=Access, info=Info}) ->
    case iorio_session:fill_session(Req, Access, Info) of
        {ok, Req1, Info1} ->
            State1 = State#state{info=Info1},
            {ok, Req1, State1};
        Error -> Error
    end.

invalid_field_response(Req, State, FieldName, FieldValue) ->
    Reason = <<"invalid-field">>,
    Body = [{type, Reason}, {field, FieldName}, {value, FieldValue}],
    Req1 = iorio_http:json_response(Req, Body),
    {false, Req1, State}.

handle_update(Req, _Info, State, undefined, _Role, _Permission) ->
    invalid_field_response(Req, State, ?FIELD_ACTION, nil);
handle_update(Req, _Info, State, _Action, undefined, _Permission) ->
    invalid_field_response(Req, State, ?FIELD_ROLE, nil);
handle_update(Req, _Info, State, _Action, _Role, undefined) ->
    invalid_field_response(Req, State, ?FIELD_PERMISSION, nil);

handle_update(Req, Info, State=#state{access=Access}, Action, Role, Permission) ->
    {ok, Info1} = ioriol_access:update_req(Info, [{action, Action},
                                                  {permission, Permission},
                                                  {role, Role}]),
    State1 = State#state{info=Info1},
    case ioriol_access:handle_req(Access, Info1) of
        ok ->
            {true, Req, State1};
        {error, Reason} ->
            lager:warning("Error processing access operation ~p", [Reason]),
            {false, Req, State1}
    end.
