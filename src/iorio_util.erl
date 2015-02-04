-module(iorio_util).
-export([reqid/0]).

reqid() -> make_ref().
