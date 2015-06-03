-module(iorio_rest_access).

-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         options/2,
         is_authorized/2,
         to_json/2,
         from_json/2]).

-ignore_xref([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         options/2,
         is_authorized/2,
         to_json/2,
         from_json/2]).

-record(state, {access, info, role, action, permission, cors}).

-include_lib("permiso/include/permiso.hrl").

-define(FIELD_PERMISSION, <<"permission">>).
-define(FIELD_ROLE, <<"role">>).
-define(FIELD_ACTION, <<"action">>).

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest};
init({ssl, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{access, Access}, {cors, Cors}]) ->
    {Bucket, Req1} = cowboy_req:binding(bucket, Req),
    {Stream, Req2} = cowboy_req:binding(stream, Req1, any),
    {ok, Info} = ioriol_access:new_req([{bucket, Bucket}, {stream, Stream}]),
	{ok, Req2, #state{access=Access, info=Info, cors=Cors}}.

options(Req, State=#state{cors=Cors}) ->
    Req1 = iorio_cors:handle_options(Req, access, Cors),
    {ok, Req1, State}.

allowed_methods(Req, State) -> {[<<"OPTIONS">>, <<"POST">>, <<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

is_authorized(Req, State=#state{access=Access, info=Info}) ->
    case fill_session(Req, State) of
        {ok, Req1, State1=#state{info=Info1}} ->
            case ioriol_access:is_authorized_for_grant(Access, Info1) of
                {ok, Info2} ->
                    {true, Req1, State1#state{info=Info2}};
                {error, Reason} ->
                    auth_error(Reason, Req1, Info, State)
            end;
        {error, Reason, Req1} ->
            auth_error(Reason, Req1, Info, State)
    end.

from_json(Req, State=#state{info=Info}) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    PLBody = iorio_json:decode_plist(Body),
    Action = proplists:get_value(?FIELD_ACTION, PLBody),
    Permission = proplists:get_value(?FIELD_PERMISSION, PLBody),
    Role = proplists:get_value(?FIELD_ROLE, PLBody),

    handle_update(Req1, Info, State, Action, Role, Permission).

to_json(Req, State=#state{access=Access, info=Info}) ->
    {ok, Resource} = ioriol_access:access_details(Access, Info),
    GrantToPList = fun ({Name, Perms}) ->
                           [{name, Name}, {grants, Perms}]
                   end,
    UsersPList = lists:map(GrantToPList, Resource#resource.user_grants),
    GroupsPList = lists:map(GrantToPList, Resource#resource.group_grants),
    AccessPList = [{users, UsersPList}, {groups, GroupsPList}],
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

handle_update(Req, Info, State=#state{access=Access}, <<"grant">>, Role, Permission) ->
    Bucket = ioriol_access:bucket(Info),
    Stream = ioriol_access:stream(Info),
    % TODO: match invalid Permission
    IPermission = ioriol_access:permission_to_internal(Bucket, Stream, Permission),
    Res = ioriol_access:grant(Access, Role, Bucket, Stream, IPermission),
    handle_access_op_result(Res, Req, State);

handle_update(Req, Info, State=#state{access=Access}, <<"revoke">>, Role, Permission) ->
    Bucket = ioriol_access:bucket(Info),
    Stream = ioriol_access:stream(Info),
    % TODO: match invalid Permission
    IPermission = ioriol_access:permission_to_internal(Bucket, Stream, Permission),
    Res = ioriol_access:revoke(Access, Role, Bucket, Stream, IPermission),
    handle_access_op_result(Res, Req, State);

handle_update(Req, _Info, State, OtherAction, _Role, _Permission) ->
    invalid_field_response(Req, State, ?FIELD_PERMISSION, OtherAction).

handle_access_op_result(ok, Req, State) ->
    Body = [{ok, true}],
    Req1 = iorio_http:json_response(Req, Body),
    {true, Req1, State};
handle_access_op_result({error, Reason}, Req, State) ->
    lager:warning("Error processing access operation ~p", [Reason]),
    Res1 = iorio_http:error(Req, <<"error">>, error_processing_operation),
    {false, Res1, State}.

auth_error(Reason, Req, Info, State) ->
    Username = ioriol_access:username(Info),
    lager:warning("unauthorized access operation user ~p reason ~p",
                  [Username, Reason]),
    Req1 = iorio_http:no_permission(Req),
    {{false, <<"jwt">>}, Req1, State}.
