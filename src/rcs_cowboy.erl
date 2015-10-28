-module(rcs_cowboy).
-export([get_permissions/1, users/0, groups/0, fold/3,
         fold_users/1, fold_users/2, fold_groups/1, fold_groups/2,
         get_security_context/1, get_user_info/1, get_group_info/1,
         group_grants/1, user_grants/1, grants_for/2, grants_to_bin/1,
         groups_grants/1,
         pgroups/1]).

-define(TOMBSTONE, '$deleted').

get_permissions(EnvKeys) ->
    {ok, Permissions} = application:get_env(riak_core, permissions),
    lists:map(fun (Key) ->
                      {Key, proplists:get_value(Key, Permissions, [])}
              end, EnvKeys).

users() ->
    Users = fold_users(fun({Username, [Options]}, Acc) ->
                               Groups = pgroups(Options),
                               [{Username, [{groups, Groups}]}|Acc]
                       end),
    {ok, Users}.

groups() ->
    Groups = fold_groups(fun({Group, [Options]}, Acc) ->
                                 Groups = pgroups(Options),
                                 [{Group, [{groups, Groups}]}|Acc]
                         end),
    {ok, Groups}.

fold(Fun, Accum, Type) ->
    riak_core_metadata:fold(fun ({_, [?TOMBSTONE]}, Acc) -> Acc;
                                (Other, Acc) -> Fun(Other, Acc)
                            end, Accum, {<<"security">>, Type}).

fold_users(Fun) -> fold_users(Fun, []).
fold_users(Fun, Accum) -> fold(Fun, Accum, <<"users">>).

fold_groups(Fun) -> fold_groups(Fun, []).
fold_groups(Fun, Accum) -> fold(Fun, Accum, <<"groups">>).

get_security_context(Username) ->
    % TODO: don't try catch
    % TODO: this is private
    try
        {ok, riak_core_security:get_context(Username)}
    catch error:badarg ->
        {error, notfound}
    end.

get_user_info(Name)  -> get_info(<<"users">>, Name).
get_group_info(Name) -> get_info(<<"groups">>, Name).

get_info(Type, Name) ->
    case riak_core_metadata:get({<<"security">>, Type}, Name) of
        undefined -> {error, notfound};
        Info -> {ok, Info}
    end.

group_grants(Name) -> grants_for(Name, <<"groupgrants">>).
user_grants(Name) -> grants_for(Name, <<"usergrants">>).

groups_grants(Names) ->
    R = fold(fun ({{Role, Target}, [Grants]}, {Groups, All}=Acc) ->
                     IsInNames = lists:member(Role, Names),
                     if Role =:= all -> {Groups, [{{global, <<"*">>}, Target, Grants}|All]};
                        IsInNames -> {[{{group, Role}, Target, Grants}|Groups], All};
                        true -> Acc
                     end
             end, {[], []}, <<"groupgrants">>),
    {Groups, All} = R,
    {ok, {grants_to_bin(Groups), grants_to_bin(All)}}.

grants_for(Name, Type) ->
    RoleType = if Type =:= <<"groupgrants">> -> group;
                  Type =:= <<"usergrants">> -> user
               end,
    R = fold(fun ({{Role, Target}, [Grants]}, Acc) when Role =:= Name; Role =:= all ->
                     [{{RoleType, Role}, Target, Grants}|Acc];
                 (_, Acc) -> Acc
             end, [], Type),
    {ok, grants_to_bin(R)}.

grants_to_bin(Grants) ->
    lists:map(fun ({Role, Bucket, Permissions}) ->
                      GrantsBin = lists:map(fun list_to_binary/1, Permissions),
                      [{grants, GrantsBin}|target_info(Role, Bucket)]
              end, Grants).

target_info({RoleType, Role}, any)  ->
    [{bucket, <<"*">>}, {key, <<"*">>}, {bucket_grant, false}, {any, true},
     {role_type, RoleType}, {role, Role}];
target_info({RoleType, Role}, Bucket) when is_binary(Bucket) ->
    [{bucket, Bucket}, {key, <<"*">>}, {bucket_grant, true}, {any, false},
     {role_type, RoleType}, {role, Role}];
target_info({RoleType, Role}, {Bucket, Key}) ->
    [{bucket, Bucket}, {key, Key}, {bucket_grant, false}, {any, false},
     {role_type, RoleType}, {role, Role}].

%name_info(all) -> [{name, <<"*">>}, {all, true}];
%name_info(Name) -> [{name, Name}, {all, false}].

pgroups(Options) -> proplists:get_value("groups", Options, []).
