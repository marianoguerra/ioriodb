-define(PERM_GET, "iorio.get").

-define(PERM_STREAM_GET, "iorio.get").
-define(PERM_STREAM_PUT, "iorio.put").
-define(PERM_STREAM_GRANT, "iorio.grant").

-define(PERM_BUCKET_GET, "iorio.get").
-define(PERM_BUCKET_PUT, "iorio.put").
-define(PERM_BUCKET_GRANT, "iorio.grant").

-define(PERM_BUCKET_LIST, "iorio.list").

-define(PERM_ADMIN_USERS, "iorio.adminusers").
-define(PERM_MAGIC_BUCKET, <<"$$iorio.permissions$$">>).

-define(DEFAULT_USER_GROUPS, ["g-authenticated"]).
-define(DEFAULT_ADMIN_GROUPS, ["g-authenticated", "g-admin"]).
-define(DEFAULT_ANONYMOUS_GROUPS, []).

-define(ALL_GROUPS, ["g-authenticated", "g-admin"]).
