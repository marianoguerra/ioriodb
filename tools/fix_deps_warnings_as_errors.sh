#!/usr/bin/env sh

sed -i 's/, warnings_as_errors//' deps/poolboy/rebar.config
sed -i 's/warnings_as_errors, //' deps/meck/rebar.config
sed -i 's/warnings_as_errors, //' deps/riak_core/rebar.config

sed -i 's/, del_source\/2/, del_source\/2, get_context\/1/' deps/riak_core/src/riak_core_security.erl

