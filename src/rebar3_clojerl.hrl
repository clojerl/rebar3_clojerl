-define(CLOJERL, <<"clojerl">>).
-define(CLJINFO_FILE, <<"cljinfo">>).

-define(DEFAULT_SRC_DIRS, ["src"]).
-define(DEFAULT_TEST_DIRS, ["test"]).

-ifdef(FUN_STACKTRACE).
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(),).
-else.
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-endif.
