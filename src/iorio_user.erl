-module(iorio_user).
-export([create/2, create/3, update/2, users/0]).

-ignore_xref([create/2, create/3, update/2, users/0]).

% NOTE '$deleted' is copied here since the other is a constant on
% riak_core_security ?TOMBSTONE
-define(TOMBSTONE, '$deleted').

-include("include/iorio.hrl").

create(Username, Password) ->
    create(Username, Password, ?DEFAULT_USER_GROUPS).

create(Username, Password, Groups) when is_binary(Username) ->
    create(binary_to_list(Username), Password, Groups);

create(Username, Password, Groups) when is_binary(Password) ->
    create(Username, binary_to_list(Password), Groups);

create(Username, Password, Groups) ->
    RcsGroups = lists:map(fun (Group) -> {"groups", [Group]} end, Groups),
    case riak_core_security:add_user(Username, [{"password", Password}]) of
        ok ->
            ok = riak_core_security:add_source([Username], {{127, 0, 0, 1}, 32}, password, []),
            ok = riak_core_security:alter_user(Username, RcsGroups),
            ok;
        Error ->
            Error
    end.

update(Username, Password) when is_binary(Username) ->
    update(binary_to_list(Username), Password);

update(Username, Password) when is_binary(Password) ->
    update(Username, binary_to_list(Password));

update(Username, Password) ->
    riak_core_security:alter_user(Username, [{"password", Password}]).

users() ->
    riak_core_metadata:fold(fun({_Username, [?TOMBSTONE]}, Acc) ->
                                    Acc;
                               ({Username, Options}, Acc) ->
                                    [{Username, Options}|Acc]
                            end, [], {<<"security">>, <<"users">>}).

