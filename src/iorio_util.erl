-module(iorio_util).
-export([reqid/0]).

reqid() -> erlang:phash2(erlang:now()).
