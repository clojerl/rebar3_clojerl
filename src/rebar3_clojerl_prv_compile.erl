-module(rebar3_clojerl_prv_compile).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, clojerl).
-define(DEPS, [{default, app_discovery}, {default, compile}]).
-define(DEFAULT_SRC_DIRS, ["src"]).

-type filename() :: filename:filename_all().

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([ {namespace,  ?NAMESPACE}
                              , {name,       ?PROVIDER}
                              , {module,     ?MODULE}
                              , {bare,       true}
                              , {deps,       ?DEPS}
                              , {example,    "rebar3 clojerl compile"}
                              , {opts,       []}
                              , {short_desc, "Compile clojerl project"}
                              , {desc,       "Compile clojerl project"}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  ok   = ensure_clojerl(State),
  Apps = lists:filter(is_not_dep_name_fun(<<"clojerl">>), all_apps(State)),
  [compile(AppInfo) || AppInfo <- Apps],
  {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p ~p", [Reason, erlang:get_stacktrace()]).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec all_apps(rebar_state:t()) -> [rebar_app_info:t()].
all_apps(State) ->
  lists:usort(rebar_state:all_deps(State)) ++ rebar_state:project_apps(State).

-spec ensure_clojerl(rebar_state:t()) -> ok.
ensure_clojerl(State) ->
  case find_dep(State, <<"clojerl">>) of
    notfound ->
      rebar_api:abort("Clojerl was not found as a dependency", []);
    {ok, DepInfo} ->
      EbinDir  = filename:join(rebar_app_info:out_dir(DepInfo), "ebin"),
      true     = code:add_patha(EbinDir),
      ok       = clojerl:start()
  end.

-spec find_dep(rebar_state:t(), binary()) -> notfound | {ok, any()}.
find_dep(State, Name) ->
  Deps  = rebar_state:all_deps(State),
  case lists:filter(is_dep_name_fun(Name), Deps) of
    [] -> notfound;
    [DepInfo] -> {ok, DepInfo}
  end.

-spec is_dep_name_fun(binary()) -> boolean().
is_dep_name_fun(Name) ->
  fun(Dep) -> Name =:= rebar_app_info:name(Dep) end.

-spec is_not_dep_name_fun(binary()) -> boolean().
is_not_dep_name_fun(Name) ->
  IsDepName = is_dep_name_fun(Name),
  fun(Dep) -> not IsDepName(Dep) end.

-spec compile(rebar_app_info:t()) -> ok.
compile(AppInfo) ->
  rebar_api:info("Clojerl Compiling ~s", [rebar_app_info:name(AppInfo)]),
  BaseDir      = rebar_app_info:out_dir(AppInfo),
  RebarOpts    = rebar_app_info:opts(AppInfo),
  CljeSrcDirs  = rebar_opts:get(RebarOpts, clje_src_dirs, ?DEFAULT_SRC_DIRS),

  OutDir       = filename:join(BaseDir, <<"ebin">>),
  Config       = [{out_dir, OutDir}],
  %% TODO: ensure dir
  ok           = code:add_pathsa([OutDir]),

  [compile_dir(BaseDir, SrcDir, OutDir, Config) || SrcDir <- CljeSrcDirs],

  ok.

-spec compile_dir(filename(), string(), filename(), [any()]) -> ok.
compile_dir(BaseDir, Dir, OutDir, Config) ->
  SrcDir   = filename:join(BaseDir, Dir),
  SrcFiles = find_files(SrcDir),

  rebar_api:debug("Source Dir ~s", [SrcDir]),
  rebar_api:debug("Source Files ~p", [SrcFiles]),
  rebar_api:debug("Out Dir ~s", [OutDir]),

  %% TODO: ensure dir
  true     = code:add_patha(SrcDir),

  [compile_file(Src, SrcDir, OutDir, Config) || Src <- SrcFiles],
  ok.

compile_file(Source, SrcDir, OutDir, Config) ->
  Target = target_file(Source, SrcDir, OutDir),
  case check_last_modified(Target, Source) of
    true  -> compile_clje(Source, Config);
    false -> skipped
  end.

-spec compile_clje(filename(), [any()]) -> ok.
compile_clje(Source, Config) ->
  rebar_api:debug("Compiling ~s...", [Source]),

  EbinDir   = proplists:get_value(out_dir, Config),
  Bindings  = #{ <<"#'clojure.core/*compile-path*">>  => EbinDir
               , <<"#'clojure.core/*compile-files*">> => true
               },
  try
    ok = 'clojerl.Var':push_bindings(Bindings),
    clj_compiler:compile_file(list_to_binary(Source))
  catch
    _:Reason ->
      rebar_api:error("~s", [clj_rt:str(Reason)]),
      Stacktrace = erlang:get_stacktrace(),
      rebar_api:debug( "Stacktrace:~n~s"
                     , [clj_utils:stacktrace(Stacktrace)]
                     )
  after
    ok = 'clojerl.Var':pop_bindings()
  end.

-spec find_files(filename()) -> [filename()].
find_files(Path) ->
  ExtRegex = "clj[ce]",
  rebar_utils:find_files(Path, ExtRegex, true).

-spec target_file(filename(), filename(), filename()) -> filename().
target_file(Source, SrcDir, OutDir) ->
  Target1   = re:replace(Source, ["^", SrcDir, "/"], "", [global]),
  Target2   = re:replace(Target1, "/", ".", [global, {return, binary}]),
  Target3   = re:replace(Target2, "_", "-", [global, {return, binary}]),
  Target4   = filename:basename(Target3, filename:extension(Source)),
  Target5   = [OutDir, "/", Target4, ".beam"],
  iolist_to_binary(Target5).

-spec check_last_modified(filename(), filename()) -> boolean().
check_last_modified(Target, Source) ->
  filelib:last_modified(Target) < filelib:last_modified(Source).
