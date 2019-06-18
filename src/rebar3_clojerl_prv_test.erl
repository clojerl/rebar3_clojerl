-module(rebar3_clojerl_prv_test).

-include("rebar3_clojerl.hrl").

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, test).
-define(NAMESPACE, clojerl).
-define(DEPS, [{clojerl, compile}]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Opts     = [ {sname, undefined, "sname", string, "Erlang node name."}
             , {ns, undefined, "ns", string, "Namespace to test."}
             , {var, undefined, "var", string, "Var to test."}
             ],
  Provider = providers:create([ {namespace,  ?NAMESPACE}
                              , {name,       ?PROVIDER}
                              , {module,     ?MODULE}
                              , {bare,       true}
                              , {deps,       ?DEPS}
                              , { example
                                , "rebar3 clojerl test "
                                  "--ns clojerl.test-clojure.control "
                                  "--var test-when"
                                }
                              , {opts,       Opts}
                              , {short_desc, "Test clojerl project"}
                              , {desc,       "Test clojerl project"}
                              , {profiles,   [test]}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  rebar_api:info("Running Clojerl tests...", []),

  {Opts, _} = rebar_state:command_parsed_args(State),
  ok        = rebar3_clojerl_utils:maybe_set_sname(Opts),

  DepsPaths = rebar_state:code_paths(State, all_deps),
  ok        = code:add_pathsa(DepsPaths),

  Apps      = rebar_state:project_apps(State),
  AppsPaths = [rebar_app_info:ebin_dir(AppInfo) || AppInfo <- Apps],
  ok        = code:add_pathsa(AppsPaths),

  %% Ensure clojerl after adding all code pahts
  ok        = rebar3_clojerl_utils:ensure_clojerl(),

  try
    [check_result(test(AppInfo, Opts)) || AppInfo <- Apps]
  catch ?WITH_STACKTRACE(_, Reason, Stacktrace)
      rebar_api:debug( "Stacktrace:~n~s"
                     , [clj_utils:format_stacktrace(Stacktrace)]
                     ),
      rebar_api:abort( "Error while testing: ~s"
                     , [clj_rt:str(Reason)]
                     )
  end,
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec test(rebar_app_info:t(), [{atom(), any()}]) -> any().
test(AppInfo, Opts) ->
  TestDirs  = rebar_app_info:get(AppInfo, clje_test_dirs, ?DEFAULT_TEST_DIRS),
  ok        = code:add_pathsa(TestDirs),
  NsOpt     = proplists:get_value(ns, Opts, undefined),
  VarOpt    = proplists:get_value(var, Opts, undefined),

  NsSyms0   = case NsOpt of
                undefined -> lists:flatmap(fun find_tests/1, TestDirs);
                NsOpt     -> [clj_rt:symbol(list_to_binary(NsOpt))]
              end,

  %% Change the current ns to a random one to avoid sticky modules or dirs
  TestNsSym = clj_rt:gensym(<<"test-ns_">>),
  'clojerl.Namespace':find_or_create(TestNsSym),
  'clojure.core':use([clj_rt:symbol(<<"clojure.core">>)]),

  %% TODO: maybe change this to a compilation of the file
  'clojure.core':require(NsSyms0),

  Var       = case {NsOpt, VarOpt} of
                {undefined, _} -> undefined;
                {_, undefined} -> undefined;
                _ ->
                  VarSymbol = clj_rt:symbol( list_to_binary(NsOpt)
                                           , list_to_binary(VarOpt)
                                           ),
                  'clojure.core':'find-var'(VarSymbol)
              end,

  case Var of
    undefined ->
      NsSyms1 = [X || X <- NsSyms0, 'clojure.core':'find-ns'(X) =/= undefined],
      rebar_api:debug("Test namespaces: ~s", [clj_rt:str(NsSyms1)]),
      'clojure.test':'run-tests'(NsSyms1);
    _         ->
      rebar_api:debug("Test var: ~s", [clj_rt:str(Var)]),
      'clojure.test':'test-var'(Var)
  end.

-spec find_tests(file:name()) -> ['clojerl.Symbol':type()].
find_tests(TestDir) ->
  [path_to_symbol(X) || X <- filelib:wildcard("**/*.clj[ce]", TestDir)].

-spec path_to_symbol(string()) -> 'clojerl.Symbol':type().
path_to_symbol(Path) ->
  Rootname    = filename:rootname(Path),
  RootnameBin = list_to_binary(Rootname),
  NsName      = clj_utils:resource_to_ns(RootnameBin),
  clj_rt:symbol(NsName).

-spec check_result(any()) -> ok.
check_result(Result) ->
  Error = clj_rt:get(Result, error),
  Fail = clj_rt:get(Result, fail),
  case Error > 0 orelse Fail > 0 of
    true  -> rebar_api:abort("Errors: ~p - Fails: ~p", [Error, Fail]);
    false -> ok
  end.
