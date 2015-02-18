-module(iorio_rk_access).
-behaviour(ioriol_access).

-export([is_authorized/4, is_authorized/5, handle/8, access_details/4,
         user_access_details/3, grant_bucket_ownership/2, add_group/1,
         authenticate/2, grant/4]).

% NOTE '$deleted' is copied here since the other is a constant on
% riak_core_security ?TOMBSTONE
-define(TOMBSTONE, '$deleted').

-include("include/iorio.hrl").

-type rk_security_ctx() :: term().

-spec handle(binary(), binary(), binary(), binary(), term(), binary(), binary(),
             binary()) -> ok | {error, term()}.
handle(_Username, _Secret, Bucket, Stream, _Session, Role, <<"grant">>, Permission) ->
    grant(Role, Bucket, Stream, Permission);
handle(_Username, _Secret, Bucket, Stream, _Session, Role, <<"revoke">>, Permission) ->
    revoke(Role, Bucket, Stream, Permission);
handle(_Username, _Secret, _Bucket, _Stream, _Session, _Role, OtherAction, _Permission) ->
    {error, {invalid_access_operation, OtherAction}}.

-spec access_details(binary(), binary(), binary(), rk_security_ctx()) ->
    {ok, ioriol_access:access_details()} | {error, term()}.
access_details(_Secret, Bucket, Stream, _Session) ->
    UserAccess = user_grants_for(Bucket, Stream),
    GroupAccess = group_grants_for(Bucket, Stream),
    FormatGrant = fun (Grant) ->
                          internal_to_permission(Bucket, Stream, Grant)
                  end,
    FormatGrants = fun ({Name, _, _, Grants}) ->
                           FormattedGrants = lists:map(FormatGrant, Grants),
                           [{name, Name}, {grants, FormattedGrants}]
                   end,
    AccessUsersJson = lists:map(FormatGrants, UserAccess),
    AccessGroupsJson = lists:map(FormatGrants, GroupAccess),
    {ok, [{users, AccessUsersJson}, {groups, AccessGroupsJson}]}.

is_authorized(_Username, Bucket, Session, Perm) ->
    check_authorized(Perm, Bucket, Session).
            
is_authorized(Username, Bucket, Stream, Session, Perm) ->
    case check_authorized(Perm, {Bucket, Stream}, Session) of
        {ok, _}=Ok -> Ok;
        _Other -> is_authorized(Username, Bucket, Session, Perm)
    end.

-spec user_access_details(binary(), binary(),rk_security_ctx()) ->
    {ok, ioriol_access:access_details()} | {error, term()}.
user_access_details(Username, _Secret, _Session) ->
    {ok, user_grants(Username)}.

grant_bucket_ownership(Username, Bucket) ->
    Permissions = [?PERM_BUCKET_GET, ?PERM_BUCKET_PUT, ?PERM_BUCKET_GRANT,
                   ?PERM_BUCKET_LIST],
    riak_core_security:add_grant([Username], Bucket, Permissions).

add_group(Name) ->
    riak_core_security:add_group(Name, []).

authenticate(Username, Password) ->
    Source = [{ip, {127, 0, 0, 1}}],
    case riak_core_security:authenticate(Username, Password, Source) of
        {ok, Ctx} ->
            Body = [{u, Username}],
            {ok, [{username, Username}, {session_body, Body}, {session, Ctx}]};
        Error ->
            lager:warning("Auth error ~p", [Error]),
            {error, unauthorized}
    end.
 
%% private
user_grants(User) ->
    Fun = fun({{Username, {Bucket, Stream}}, [Perms]}, Acc) when User == Username->
                  [{Username, Bucket, Stream, Perms}|Acc];
             ({{Username, Bucket}, [Perms]}, Acc) when User == Username ->
                  [{Username, Bucket, any, Perms}|Acc];
             (_, Acc) ->
                  Acc
          end,
    riak_core_metadata:fold(Fun, [], {<<"security">>, <<"usergrants">>}).

%users_grants() ->
%    Fun = fun({{Username, {Bucket, Stream}}, [Perms]}, Acc) ->
%                  [{Username, Bucket, Stream, Perms}|Acc];
%             ({{Username, Bucket}, [Perms]}, Acc) ->
%                  [{Username, Bucket, any, Perms}|Acc]
%          end,
%    riak_core_metadata:fold(Fun, [], {<<"security">>, <<"usergrants">>}).


grants_for(QBucket, any, Type) ->
    riak_core_metadata:fold(fun({_, [?TOMBSTONE]}, Acc) -> Acc;
                                ({{Name, Bucket}, [Perms]}, Acc) when QBucket =:= Bucket ->
                                    [{Name, Bucket, any, Perms}|Acc];
                               (_, Acc) -> Acc
                            end, [], {<<"security">>, Type});

grants_for(QBucket, QStream, Type) ->
    riak_core_metadata:fold(fun({_, [?TOMBSTONE]}, Acc) -> Acc;
                               ({{Name, {Bucket, Stream}}, [Perms]}, Acc) 
                                 when QBucket =:= Bucket andalso QStream =:= Stream ->
                                    [{Name, Bucket, Stream, Perms}|Acc];
                               (_, Acc) -> Acc
                            end, [], {<<"security">>, Type}).

group_grants_for(QBucket, QStream) ->
    grants_for(QBucket, QStream, <<"groupgrants">>).

user_grants_for(QBucket, QStream) ->
    grants_for(QBucket, QStream, <<"usergrants">>).

check_authorized(Perm, Thing, Session) ->
    case riak_core_security:check_permissions({Perm, Thing}, Session) of
        %% XXX not updateing session
        {true, NewSession} ->
            {ok, [{session, NewSession}]};
        _Other ->
            {error, unauthorized}
    end.

internal_to_permission(_Bucket, any, ?PERM_BUCKET_GET) -> <<"get">>;
internal_to_permission(_Bucket, any, ?PERM_BUCKET_PUT) -> <<"put">>;
internal_to_permission(_Bucket, any, ?PERM_BUCKET_LIST) -> <<"list">>;
internal_to_permission(_Bucket, any, ?PERM_BUCKET_GRANT) -> <<"grant">>;

internal_to_permission(_Bucket, _Stream, ?PERM_STREAM_GET) -> <<"get">>;
internal_to_permission(_Bucket, _Stream, ?PERM_STREAM_PUT) -> <<"put">>;
internal_to_permission(_Bucket, _Stream, ?PERM_STREAM_GRANT) -> <<"grant">>;

internal_to_permission(_Bucket, _Stream, ?PERM_ADMIN_USERS) -> <<"adminusers">>.


%permission_to_internal(_Bucket, any, <<"get">>) -> ?PERM_BUCKET_GET;
%permission_to_internal(_Bucket, any, <<"put">>) -> ?PERM_BUCKET_PUT;
%permission_to_internal(_Bucket, any, <<"list">>) -> ?PERM_BUCKET_LIST;
%permission_to_internal(_Bucket, any, <<"grant">>) -> ?PERM_BUCKET_GRANT;
%
%permission_to_internal(_Bucket, _Stream, <<"get">>) -> ?PERM_STREAM_GET;
%permission_to_internal(_Bucket, _Stream, <<"put">>) -> ?PERM_STREAM_PUT;
%permission_to_internal(_Bucket, _Stream, <<"grant">>) -> ?PERM_STREAM_GRANT;
%
%permission_to_internal(_Bucket, _Stream, <<"adminusers">>) -> ?PERM_ADMIN_USERS.
%
grant(<<"*">>, Bucket, any, Permission) ->
    riak_core_security:add_grant(all, Bucket, [Permission]);

grant(Username, Bucket, any, Permission) ->
    riak_core_security:add_grant([Username], Bucket, [Permission]);

grant(<<"*">>, Bucket, Stream, Permission) ->
    riak_core_security:add_grant(all, {Bucket, Stream}, [Permission]);

grant(Username, Bucket, Stream, Permission) ->
    riak_core_security:add_grant([Username], {Bucket, Stream}, [Permission]).

revoke(<<"*">>, Bucket, any, Permission) ->
    riak_core_security:add_revoke(all, Bucket, [Permission]);

revoke(Username, Bucket, any, Permission) ->
    riak_core_security:add_revoke([Username], Bucket, [Permission]);

revoke(<<"*">>, Bucket, Stream, Permission) ->
    riak_core_security:add_revoke(all, {Bucket, Stream}, [Permission]);

revoke(Username, Bucket, Stream, Permission) ->
    riak_core_security:add_revoke([Username], {Bucket, Stream}, [Permission]).

