-module(iorio_user).
-export([create/2]).

create(Username, Password) when is_binary(Username) ->
    create(binary_to_list(Username), Password);

create(Username, Password) when is_binary(Password) ->
    create(Username, binary_to_list(Password));

create(Username, Password) ->
    case riak_core_security:add_user(Username, [{"password", Password}]) of
        ok ->
            riak_core_security:add_source([Username], {{127, 0, 0, 1}, 32}, password, []);
        Error ->
            Error
    end.


