-module(dummy_iorio).
-compile(export_all).

-include_lib("sblob/include/sblob.hrl").

subscribe(_Bucket, _Stream, _Pid) -> ok.
unsubscribe(_Bucket, _Stream, _Pid) -> ok.
get(_Bucket, _Stream, _From, _Limit) -> [].
put(_Bucket, _Stream, Body) -> #sblob_entry{data=Body}.
