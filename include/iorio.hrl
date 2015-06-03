-define(PERM_GET, "iorio.get").

-define(PERM_STREAM_GET, "iorio.get").
-define(PERM_STREAM_PUT, "iorio.put").
-define(PERM_STREAM_GRANT, "iorio.grant").

-define(PERM_BUCKET_GET, "iorio.get").
-define(PERM_BUCKET_PUT, "iorio.put").
-define(PERM_BUCKET_GRANT, "iorio.grant").

-define(PERM_BUCKET_LIST, "iorio.list").

-define(PERM_ADMIN_USERS, "iorio.adminusers").
-define(PERM_VIEW_STATS, "iorio.viewstats").
-define(PERM_MAGIC_BUCKET, <<"$$iorio.permissions$$">>).

-define(USER_GROUP, "g-authenticated").
-define(ADMIN_GROUP, "g-admin").

-define(ALL_GROUPS, ["g-authenticated", "g-admin"]).

-record(iorio_cors, {origins=[] :: list(binary()),
                     headers=[] :: list(binary()),
                     max_age_secs = <<"60">> :: binary(),
                     csv_headers = <<"">> :: binary(),
                     enabled=false :: boolean()}).
