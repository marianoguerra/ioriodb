#!/usr/bin/env sh

RCS=deps/riak_core/src/riak_core_security.erl
sed -i 's/, del_source\/2/, del_source\/2, get_context\/1/' $RCS
sed -i 's/bucket2iolist({Type, Bucket}) ->/bucket2iolist(any) -> <<"*">>;\n&/'  $RCS

