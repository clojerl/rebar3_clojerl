-module(rebar3_clojerl_prv_compile).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, clojerl).
-define(DEPS, [{default, compile}]).
-define(DEFAULT_SRC_DIRS, ["src"]).

-define(CLOJERL, <<"clojerl">>).

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
  DepsPaths = rebar_state:code_paths(State, all_deps),
  ok        = code:add_pathsa(DepsPaths),

  ok        = ensure_clojerl(State),

  AllApps   = all_apps(State),
  Apps      = lists:filter(is_not_dep_name_fun(?CLOJERL), AllApps),
  Config    = [{protocols_out_dir, protocols_out_dir(State)}],

  [compile(AppInfo, Config) || AppInfo <- Apps],
  clean_duplicates(AllApps, Config),
  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec all_apps(rebar_state:t()) -> [rebar_app_info:t()].
all_apps(State) ->
  lists:usort(rebar_state:all_deps(State)) ++ rebar_state:project_apps(State).

-spec protocols_out_dir(rebar_state:t()) -> file:name().
protocols_out_dir(State) ->
  %% Get the first application from this project.
  [AppInfo | _] = rebar_state:project_apps(State),
  ProtoDir      = rebar_app_info:ebin_dir(AppInfo),
  rebar_api:debug("Protocols dir: ~s", [ProtoDir]),
  ProtoDir.

-spec clean_duplicates([rebar_app_info:t()], [any()]) -> ok.
clean_duplicates(Apps, Config) ->
  ProtoDir      = proplists:get_value(protocols_out_dir, Config),
  BeamFilepaths = rebar_utils:find_files(ProtoDir, ".beam$"),
  BeamFilenames = [filename:basename(F) || F <- BeamFilepaths],

  rebar_api:debug( "Finding duplicates for:~n~p", [BeamFilenames]),

  Dirs    = [rebar_app_info:ebin_dir(App) || App <- Apps] -- [ProtoDir],
  rebar_api:debug( "Searching duplicates in:~n~p", [Dirs]),
  Deleted = [clean_duplicates_from_dir(BeamFilenames, Dir) || Dir <- Dirs],

  ok      = lists:foreach(fun update_app_file/1, Deleted).

-spec clean_duplicates_from_dir([file:name()], file:name()) ->
  {file:name(), [file:name()]}.
clean_duplicates_from_dir(BeamFilenames, Dir) ->
  Filepaths = [ begin
                  rebar_api:debug("Deleted duplicate ~s", [F]),
                  ok = file:delete(F),
                  F
                end
                || F <- rebar_utils:find_files(Dir, ".beam$"),
                   lists:member(filename:basename(F), BeamFilenames)
              ],
  {Dir, Filepaths}.

-spec update_app_file({file:name(), [file:name()]}) -> ok.
update_app_file({_Dir, []}) ->
  ok;
update_app_file({Dir, Filepaths}) ->
  case rebar_utils:find_files(Dir, ".app$", false) of
    [AppFile] ->
      DeletedModules = [ list_to_atom(filename:basename(Path, ".beam"))
                         || Path <- Filepaths
                       ],
      {ok, [{application, AppName, AppDetail0}]} = file:consult(AppFile),
      rebar_api:debug("Updating app file for ~p", [AppName]),
      rebar_api:debug("Removing modules:~n~p", [DeletedModules]),
      AppModules0 = proplists:get_value(modules, AppDetail0, []),
      AppModules1 = AppModules0 -- DeletedModules,

      AppDetail1  = lists:keyreplace( modules
                                    , 1
                                    , AppDetail0
                                    , {modules, AppModules1}
                                    ),
      Spec = io_lib:format("~p.\n", [{application, AppName, AppDetail1}]),
      ok = rebar_file_utils:write_file_if_contents_differ(AppFile, Spec, utf8),
      ok;
    [] -> ok
  end.

-spec ensure_clojerl(rebar_state:t()) -> ok.
ensure_clojerl(State) ->
  case find_dep(State, ?CLOJERL) of
    notfound ->
      rebar_api:abort("Clojerl was not found as a dependency", []);
    {ok, _} ->
      ok = clojerl:start()
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

-spec compile(rebar_app_info:t(), [any()]) -> boolean().
compile(AppInfo, Config0) ->
  case find_files_to_compile(AppInfo) of
    [] ->
      false;
    SrcFiles ->
      rebar_api:info("Clojerl Compiling ~s", [rebar_app_info:name(AppInfo)]),
      EbinDir      = rebar_app_info:ebin_dir(AppInfo),
      Config1      = [{out_dir, EbinDir} | Config0],
      [compile_clje(Src, Config1) || Src <- SrcFiles],
      true
  end.

-spec compile_clje(file:name(), [any()]) -> ok.
compile_clje(Source, Config) ->
  rebar_api:debug("Compiling ~s...", [Source]),

  EbinDir   = list_to_binary(proplists:get_value(out_dir, Config)),
  ProtoDir  = list_to_binary(proplists:get_value(protocols_out_dir, Config)),
  Bindings  = #{ <<"#'clojure.core/*compile-files*">>          => true
               , <<"#'clojure.core/*compile-path*">>           => EbinDir
               , <<"#'clojure.core/*compile-protocols-path*">> => ProtoDir
               },
  try
    ok = 'clojerl.Var':push_bindings(Bindings),
    clj_compiler:compile_file(list_to_binary(Source))
  catch
    _:Reason ->
      Stacktrace = erlang:get_stacktrace(),
      rebar_api:debug( "Stacktrace:~n~s"
                     , [clj_utils:stacktrace(Stacktrace)]
                     ),
      rebar_api:abort( "Error while compiling ~s: ~s"
                     , [Source, clj_rt:str(Reason)]
                     )
  after
    ok = 'clojerl.Var':pop_bindings()
  end.

%% =============================================================================
%% Find files to compile

-spec find_files_to_compile(rebar_app_info:t()) -> [file:name()].
find_files_to_compile(AppInfo) ->
  OutDir      = rebar_app_info:out_dir(AppInfo),
  EbinDir     = rebar_app_info:ebin_dir(AppInfo),
  CljeSrcDirs = rebar_app_info:get(AppInfo, clje_src_dirs, ?DEFAULT_SRC_DIRS),

  SrcDirPaths = [filename:join(OutDir, Dir) || Dir <- CljeSrcDirs],
  ok          = code:add_pathsa(SrcDirPaths),
  Fun         = fun(SrcDir) -> find_files_to_compile(SrcDir, EbinDir) end,
  lists:usort(lists:flatmap(Fun, SrcDirPaths)).

-spec find_files_to_compile(file:name(), file:name()) -> ok.
find_files_to_compile(SrcDir, EbinDir) ->
  SrcFiles = rebar_utils:find_files(SrcDir, "clj[ce]"),
  [Source || Source <- SrcFiles, should_compile_file(Source, SrcDir, EbinDir)].

-spec should_compile_file(file:name(), file:name(), file:name()) -> boolean().
should_compile_file(Source, SrcDir, EbinDir) ->
  Target = target_file(Source, SrcDir, EbinDir),
  should_compile(Target, Source).

-spec target_file(file:name(), file:name(), file:name()) -> file:name().
target_file(Src, SrcDir, OutDir) ->
  Target1 = re:replace(Src, ["^", SrcDir, "/"], "", [global, {return, binary}]),
  Target2 = clj_utils:resource_to_ns(Target1),
  Target3 = filename:basename(Target2, filename:extension(Src)),
  Target4 = [OutDir, "/", Target3, ".beam"],
  iolist_to_binary(Target4).

-spec should_compile(file:name(), file:name()) -> boolean().
should_compile(Target, Source) ->
  not filelib:is_file(Target)
    orelse filelib:last_modified(Target) < filelib:last_modified(Source).
