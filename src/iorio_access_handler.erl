-module(iorio_access_handler).

-export([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         is_authorized/2,
         to_json/2,
         from_json/2
        ]).

-include("include/iorio.hrl").
-record(state, {secret, session, bucket, stream}).

-define(FIELD_PERMISSION, <<"permission">>).
-define(FIELD_USERNAME, <<"username">>).
-define(FIELD_ACTION, <<"action">>).

init({tcp, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest};
init({ssl, http}, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{secret, Secret}]) ->
    {Bucket, Req1} = cowboy_req:binding(bucket, Req),
    {Stream, Req2} = cowboy_req:binding(stream, Req1, any),
	{ok, Req2, #state{secret=Secret, bucket=Bucket, stream=Stream}}.

allowed_methods(Req, State) -> {[<<"POST">>, <<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

is_authorized(Req, State=#state{secret=Secret, bucket=Bucket, stream=Stream}) ->
    GetSession = fun get_session/1,
    SetSession = fun set_session/2,
    check_auth(Req, Secret, State, GetSession, SetSession, Bucket, Stream).

from_json(Req, State=#state{bucket=Bucket, stream=Stream}) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    Info = iorio_json:decode_plist(Body),
    Action = proplists:get_value(?FIELD_ACTION, Info, notfound),
    Permission = proplists:get_value(?FIELD_PERMISSION, Info),
    Username = proplists:get_value(?FIELD_USERNAME, Info, notfound),
    RealPermission = iorio_session:permission_to_internal(Bucket, Stream,
                                                          Permission),
    handle_access_action(Action, Username, Bucket, Stream, RealPermission,
                         Req1, State, Permission).

to_json(Req, State=#state{bucket=Bucket, stream=Stream}) ->
    UserAccess = iorio_user:user_grants_for(Bucket, Stream),
    GroupAccess = iorio_user:group_grants_for(Bucket, Stream),
    FormatGrant = fun (Grant) ->
                          iorio_session:internal_to_permission(Bucket, Stream, Grant)
                  end,
    FormatGrants = fun ({Name, _, _, Grants}) ->
                           FormattedGrants = lists:map(FormatGrant, Grants),
                           [{name, Name}, {grants, FormattedGrants}]
                   end,
    AccessUsersJson = lists:map(FormatGrants, UserAccess),
    AccessGroupsJson = lists:map(FormatGrants, GroupAccess),
    AccessJsonStr = iorio_json:encode([{users, AccessUsersJson}, {groups, AccessGroupsJson}]),
    {AccessJsonStr, Req, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.

% private api

check_auth(Req, Secret, State, GetSession, SetSession, Bucket, any) ->
    iorio_session:handle_is_authorized_for_bucket(Req, Secret, State,
                                                  GetSession, SetSession,
                                                  Bucket, ?PERM_BUCKET_GRANT);

check_auth(Req, Secret, State, GetSession, SetSession, Bucket, Stream) ->
    iorio_session:handle_is_authorized_for_stream(Req, Secret, State,
                                                  GetSession, SetSession,
                                                  Bucket, Stream,
                                                  ?PERM_STREAM_GRANT).


get_session(#state{session=Session}) -> Session.
set_session(State, Session) -> State#state{session=Session}.

invalid_field_response(Req, FieldName, FieldValue) ->
    Reason = <<"invalid-field">>,
    Body = [{type, Reason}, {field, FieldName}, {value, FieldValue}],
    iorio_http:json_response(Req, Body).


handle_access_action(notfound, _Username, _Bucket, _Stream, _RealPermission,
                     Req, State, _Permission) ->
    Req1 = invalid_field_response(Req, ?FIELD_ACTION, nil),
    {false, Req1, State};

handle_access_action(_Action, notfound, _Bucket, _Stream, _RealPemission, Req,
                     State, _Permission) ->
    Req1 = invalid_field_response(Req, ?FIELD_USERNAME, nil),
    {false, Req1, State};

handle_access_action(_Action, _Username, _Bucket, _Stream, notfound, Req,
                     State, Permission) ->
    Req1 = invalid_field_response(Req, ?FIELD_PERMISSION, Permission),
    {false, Req1, State};

handle_access_action(<<"grant">>, Username, Bucket, Stream, RealPermission,
                     Req1, State, Permission) ->
    grant_access(Username, Bucket, Stream, RealPermission, Req1, State, Permission);

handle_access_action(<<"revoke">>, Username, Bucket, Stream, RealPermission,
                     Req1, State, Permission) ->
    revoke_access(Username, Bucket, Stream, RealPermission, Req1, State, Permission);

handle_access_action(Other, _Username, _Bucket, _Stream, _RealPermission,
                     Req, State, _Permission) ->
    Req1 = invalid_field_response(Req, ?FIELD_ACTION, Other),
    {false, Req1, State}.


revoke_access(Username, Bucket, Stream, RealPermission, Req, State, _Permission) ->
    Result = iorio_session:revoke(Username, Bucket, Stream, RealPermission),
    grant_result_to_response(Result, Req, State).


grant_access(Username, Bucket, Stream, RealPermission, Req, State, _Permission) ->
    Result = iorio_session:grant(Username, Bucket, Stream, RealPermission),
    grant_result_to_response(Result, Req, State).


grant_result_to_response({error, Error}, Req, State) ->
    grant_result_to_response({errors, [Error]}, Req, State);

grant_result_to_response({errors, Errors}, Req, State) ->
    Req1 = iorio_http:json_response(Req, [{type, <<"error">>},
                                          {value, Errors}]),
    {false, Req1, State};

grant_result_to_response(ok, Req, State) ->
    Req1 = iorio_http:ok(Req),
    {true, Req1, State}.
