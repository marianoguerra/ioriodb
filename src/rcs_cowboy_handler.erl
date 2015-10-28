-module(rcs_cowboy_handler).
-export([init/3, terminate/3]).

-ignore_xref([init/3, terminate/3]).

-export([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         resource_exists/2,
         is_authorized/2,
         delete_resource/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         to_json/2]).

-ignore_xref([rest_init/2,
         rest_terminate/2,
         allowed_methods/2,
         resource_exists/2,
         is_authorized/2,
         delete_resource/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         to_json/2]).

-record(state, {env_keys, action, json_encoder, json_decoder, param1,
                user_ctx, group_info, is_authorized, method, base_uri}).

init(_, _Req, _Opts) -> {upgrade, protocol, cowboy_rest}.

rest_init(Req, #{env_keys := EnvKeys, is_authorized := IsAuthorizedFun,
                 json_encoder := JsonEncoder, json_decoder := JsonDecoder,
                 base_uri := BaseUri}) ->
    {Action, Req1} = cowboy_req:binding(action, Req),
    {Param1, Req2} = cowboy_req:binding(param1, Req1),
    {Method, Req3} = method(Req2),
    {ok, Req3, #state{env_keys=EnvKeys, method=Method,
                      action=Action, param1=Param1,
                      is_authorized=IsAuthorizedFun, base_uri=BaseUri,
                      json_encoder=JsonEncoder, json_decoder=JsonDecoder}}.

allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, to_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, from_json}], Req, State}.

is_authorized(Req, State=#state{action=Action, param1=Param1,
                                is_authorized=IsAuthorizedFun}) ->
    Info = [{action, Action}, {param1, Param1}],
    {IsAuthorized, NewReq} = IsAuthorizedFun(Req, Info),
    {IsAuthorized, NewReq, State}.

to_json(Req, State=#state{action = <<"permissions">>, env_keys=EnvKeys}) ->
    Permissions = rcs_cowboy:get_permissions(EnvKeys),
    to_json_reply(Req, State, Permissions);
to_json(Req, State=#state{action = <<"users">>, param1=undefined}) ->
    {ok, Users} = rcs_cowboy:users(),
    to_json_reply(Req, State,[{users, Users}]);
to_json(Req, State=#state{action = <<"groups">>, param1=undefined}) ->
    {ok, Groups} = rcs_cowboy:groups(),
    to_json_reply(Req, State,[{groups, Groups}]);
to_json(Req, State=#state{action = <<"groups">>, param1=Group,
                          group_info=GroupInfo}) ->
    Groups = rcs_cowboy:pgroups(GroupInfo),
    {ok, DirectGrants} = rcs_cowboy:group_grants(Group),
    {ok, {GroupsGrants, GlobalGrants}} = rcs_cowboy:groups_grants(Groups),
    Response = [{group, [{name, Group}, {groups, Groups},
                         {grants, [{direct, DirectGrants},
                                   {groups, GroupsGrants},
                                   {global, GlobalGrants}]}]}],
    to_json_reply(Req, State, Response);
to_json(Req, State=#state{action = <<"users">>, user_ctx=UserCtx}) ->
    {context, Username, _CtxGrants, _Ts} = UserCtx,
    {ok, UserInfo} = rcs_cowboy:get_user_info(Username),
    {ok, DirectGrants} = rcs_cowboy:user_grants(Username),
    Groups = rcs_cowboy:pgroups(UserInfo),
    {ok, {GroupsGrants, GlobalGrants}} = rcs_cowboy:groups_grants(Groups),
    Response = [{user, [{username, Username}, {groups, Groups},
                         {grants, [{direct, DirectGrants},
                                   {groups, GroupsGrants},
                                   {global, GlobalGrants}]}]}],
    to_json_reply(Req, State, Response).

delete_resource(Req, State=#state{action = <<"users">>, param1=Username}) ->
    handle_delete(Req, State, riak_core_security:del_user(Username));
delete_resource(Req, State=#state{action = <<"groups">>, param1=Groupname}) ->
    handle_delete(Req, State, riak_core_security:del_group(Groupname)).

from_json(Req, State=#state{action = <<"groups">>, method=put}) ->
    with_group_body(Req, State, fun do_update_group/5);
from_json(Req, State=#state{action = <<"groups">>}) ->
    with_group_body(Req, State, fun do_create_group/5);
from_json(Req, State=#state{action = <<"users">>, method=put}) ->
    with_user_body(Req, State, fun do_update_user/6);
from_json(Req, State=#state{action = <<"users">>}) ->
    with_user_body(Req, State, fun do_create_user/6);
from_json(Req, State=#state{action = <<"grants">>}) ->
    with_grant_body(Req, State, fun do_grant/6);
from_json(Req, State=#state{action = <<"revokes">>}) ->
    with_grant_body(Req, State, fun do_revoke/6);
from_json(Req, State=#state{}) ->
    {false, Req, State}.

% TODO: load the body here and check if it exists
resource_exists(Req, State=#state{action = <<"users">>, method = post}) ->
    {false, Req, State};
resource_exists(Req, State=#state{action = <<"groups">>, method = post}) ->
    {false, Req, State};

resource_exists(Req, State=#state{action = <<"users">>, param1=Username})
  when is_binary(Username) ->
    case rcs_cowboy:get_security_context(Username) of
        {error, notfound} ->
            {false, Req, State};
        {ok, Ctx} ->
            {true, Req, State#state{user_ctx=Ctx}}
    end;
resource_exists(Req, State=#state{action = <<"groups">>, param1=Group})
  when is_binary(Group) ->
    case rcs_cowboy:get_group_info(Group) of
        {error, notfound} ->
            {false, Req, State};
        {ok, Info} ->
            {true, Req, State#state{group_info=Info}}
    end;
resource_exists(Req, State=#state{action=Action, param1=Param1}) ->
    {exists(Action, Param1), Req, State}.

rest_terminate(_Req, _State) ->
	ok.

terminate(_Reason, _Req, _State) ->
	ok.

% private

to_json_reply(Req, State=#state{json_encoder=JsonEncoder}, Result) ->
    {JsonEncoder(Result), Req, State}.

exists(<<"permissions">>, undefined) -> true;
exists(<<"users">>, undefined) -> true;
exists(<<"groups">>, undefined) -> true;
exists(<<"users">>, User) when is_binary(User) -> true;
exists(<<"groups">>, User) when is_binary(User) -> true;
exists(_Action, _Param) -> false.

method(Req) ->
    {Method, Req1} = cowboy_req:method(Req),
    AMethod = case Method of
                  <<"POST">> -> post;
                  <<"GET">> -> get;
                  <<"PUT">> -> put;
                  <<"DELETE">> -> delete;
                  <<"OPTIONS">> -> options;
                  <<"HEAD">> -> head
              end,
    {AMethod, Req1}.

parse_body(Req, State=#state{json_decoder=JsonDecoder}) ->
    {ok, BodyRaw, Req1} = cowboy_req:body(Req),
    try
        Body = JsonDecoder(BodyRaw),
        {ok, Body, Req1, State}
    catch
        T:E -> {error, {T, E}, Req1, State}
    end.

create_user(Username, Password, Groups) ->
    Options = [{"password", binary_to_list(Password)},
               {"groups", groups_to_rc(Groups)}],
    case riak_core_security:add_user(Username, Options) of
        ok ->
            % TODO: add config function to decide source
            Source = {{127, 0, 0, 1}, 32},
            riak_core_security:add_source([Username], Source, password, []);
        Error ->
            Error
    end.

update_user(Username, undefined, Groups) ->
    Options = [{"groups", groups_to_rc(Groups)}],
    lager:info("update user ~p no password ~p", [Username, Groups]),
    riak_core_security:alter_user(Username, Options);
update_user(Username, Password, Groups) ->
    Options = [{"password", binary_to_list(Password)},
               {"groups", groups_to_rc(Groups)}],
    lager:info("update user ~p password ~p ~p", [Username, Password, Groups]),
    riak_core_security:alter_user(Username, Options).

create_group(Groupname, Groups) ->
    Options = [{"groups", groups_to_rc(Groups)}],
    riak_core_security:add_group(Groupname, Options).

update_group(Groupname, Groups) ->
    Options = [{"groups", groups_to_rc(Groups)}],
    riak_core_security:alter_group(Groupname, Options).

groups_to_rc(Groups) ->
    string:join(lists:map(fun binary_to_list/1, Groups), ",").

json_ok_response(Req) ->
    json_response(Req, <<"{}">>).

json_response(Req, JsonBody) ->
    ContentType = <<"application/json">>,
    set_content_type_body(Req, ContentType, JsonBody).

set_content_type(Req, ContentType) ->
    Header = <<"Content-Type">>,
    Req1 = cowboy_req:delete_resp_header(<<"content-type">>, Req),
    cowboy_req:set_resp_header(Header, ContentType, Req1).

set_content_type_body(Req, ContentType, Body) ->
    Req1 = set_content_type(Req, ContentType),
    response(Req1, Body).

response(Req, Body) ->
    cowboy_req:set_resp_body(Body, Req).

reply_created(BaseUri, Path1, Groupname, Req, State) ->
    UriStr = io_lib:format("~s/~s/~s", [BaseUri, Path1, Groupname]),
    {{true, UriStr}, json_ok_response(Req), State}.

handle_delete(Req, State, ok) -> {true, Req, State};
handle_delete(Req, State=#state{action=Action, param1=Param1}, Other) ->
    lager:error("deleting ~p ~p: ~p", [Action, Param1, Other]),
    {false, Req, State}.

with_group_body(Req, State, Fun) ->
    case parse_body(Req, State) of
        {ok, #{<<"groupname">> := Groupname, <<"groups">> := Groups}=Body,
         Req1, State1} when is_list(Groups), is_binary(Groupname) ->
            Fun(Req1, State1, Body, Groupname, Groups);
        {ok, _Body, Req1, State1} ->
            {false, Req1, State1};
        {error, _Reason, Req1, State1} ->
            {false, Req1, State1}
    end.

with_user_body(Req, State, Fun) ->
    case parse_body(Req, State) of
        {ok, #{<<"username">> := Username, <<"groups">> := Groups}=Body,
         Req1, State1}
          when is_list(Groups), is_binary(Username) ->
            Password = maps:get(<<"password">>, Body, undefined),
            if is_binary(Password) orelse Password =:= undefined ->
                   Fun(Req1, State1, Body, Username, Password, Groups);
               true ->
                   {false, Req1, State1}
            end;
        {ok, _Body, Req1, State1} ->
            {false, Req1, State1};
        {error, _Reason, Req1, State1} ->
            {false, Req1, State1}
    end.

with_grant_body(Req, State, Fun) ->
    case parse_body(Req, State) of
        {ok, #{<<"role">> := Role, <<"bucket">> := Bucket, <<"key">> := Key,
               <<"permission">> := Permission}=Body,
         Req1, State1}
          when is_binary(Role), is_binary(Bucket), is_binary(Key), is_binary(Permission) ->
            Fun(Req1, State1, Body, to_rc_role(Role),
                to_rc_bucket(Bucket, Key), to_rc_permission(Permission));
        {ok, _Body, Req1, State1} ->
            {false, Req1, State1};
        {error, _Reason, Req1, State1} ->
            {false, Req1, State1}
    end.

do_update(Req, State, Body, ErrorMsg, Fun) ->
    try Fun() of
        ok ->
            {true, json_ok_response(Req), State};
        Error ->
            lager:error("~s ~p: ~p", [ErrorMsg, Body, Error]),
            {false, Req, State}
    catch
        T:E ->
            lager:error("~s ~p: ~p", [ErrorMsg, T, E]),
            {false, Req, State}
    end.

do_create(Req, State=#state{base_uri=BaseUri}, Body, ErrorMsg, Uri1, Uri2, Fun) ->
    try Fun() of
        ok ->
            reply_created(BaseUri, Uri1, Uri2, Req, State);
        Error ->
            lager:error("~s ~p: ~p", [ErrorMsg, Body, Error]),
            {false, Req, State}
    catch
        T:E ->
            lager:error("~s ~p: ~p", [ErrorMsg, T, E]),
            {false, Req, State}
    end.

do_grant(Req, State, Body, Role, Bucket, Permission) ->
    do_update(Req, State, Body, "granting permission",
              fun () ->
                      riak_core_security:add_grant(Role, Bucket, Permission)
              end).

do_revoke(Req, State, Body, Role, Bucket, Permission) ->
    do_update(Req, State, Body, "revoking permission",
              fun () ->
                      riak_core_security:add_revoke(Role, Bucket, Permission)
              end).
do_create_group(Req, State, Body, Groupname, Groups) ->
    do_create(Req, State, Body, "creating group", "groups", Groupname,
              fun () -> create_group(Groupname, Groups) end).

do_update_group(Req, State, Body, Groupname, Groups) ->
    do_update(Req, State, Body, "updating group",
              fun () -> update_group(Groupname, Groups) end).

do_create_user(Req, State, Body, Username, Password, Groups) ->
    do_create(Req, State, Body, "creating user", "users", Username,
              fun () -> create_user(Username, Password, Groups) end).

do_update_user(Req, State, Body, Username, Password, Groups) ->
    do_update(Req, State, Body, "updating user",
              fun () -> update_user(Username, Password, Groups) end).

to_rc_bucket(Bucket, <<"*">>) -> Bucket;
to_rc_bucket(Bucket, Key) -> {Bucket, Key}.

to_rc_role(<<"*">>) -> all;
to_rc_role(Role) -> [Role].

to_rc_permission(Permission) -> [binary_to_list(Permission)].
