-module(rebar3_clojerl_prv_compile).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, clojerl).
-define(DEPS, [{default, compile}]).
-define(DEFAULT_SRC_DIRS, ["src"]).

-define(CLOJERL, <<"clojerl">>).
-define(CLJINFO_FILE, <<"cljinfo">>).

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
  Config    = #{protocols_dir => protocols_dir(State)},

  restore_duplicates(AllApps),
  [compile(AppInfo, Config) || AppInfo <- Apps],
  backup_duplicates(AllApps, Config),

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

-spec protocols_dir(rebar_state:t()) -> file:name().
protocols_dir(State) ->
  %% Get the first application from this project.
  [AppInfo | _] = rebar_state:project_apps(State),
  ProtoDir      = rebar_app_info:ebin_dir(AppInfo),
  rebar_api:debug("Protocols dir: ~s", [ProtoDir]),
  ProtoDir.

-spec backup_duplicates([rebar_app_info:t()], [any()]) -> ok.
backup_duplicates(Apps, Config) ->
  ProtoDir      = maps:get(protocols_dir, Config),
  BeamFilepaths = rebar_utils:find_files(ProtoDir, ".beam$"),
  BeamFilenames = [filename:basename(F) || F <- BeamFilepaths],

  Dirs    = [rebar_app_info:ebin_dir(App) || App <- Apps] -- [ProtoDir],
  rebar_api:debug("Finding duplicates for:~n~p~nin~n~p", [BeamFilenames, Dirs]),
  Deleted = [backup_duplicates_from_dir(BeamFilenames, Dir) || Dir <- Dirs],

  ok      = lists:foreach(fun update_app_file/1, Deleted).

-spec backup_duplicates_from_dir([file:name()], file:name()) ->
  {file:name(), [file:name()]}.
backup_duplicates_from_dir(BeamFilenames, Dir) ->
  Filepaths = [ begin
                  rebar_api:debug("Backup duplicate ~s", [F]),
                  ok = file:rename(F, F ++ ".backup"),
                  F
                end
                || F <- rebar_utils:find_files(Dir, ".beam$"),
                   lists:member(filename:basename(F), BeamFilenames)
              ],
  {Dir, Filepaths}.

-spec restore_duplicates([rebar_app_info:t()]) -> ok.
restore_duplicates(Apps) ->
  Dirs = [rebar_app_info:ebin_dir(App) || App <- Apps],
  [ begin
      DestPath = filename:rootname(Path),
      ok       = file:rename(Path, DestPath),
      rebar_api:debug("Restored duplicate ~s", [DestPath])
    end
    || Dir  <- Dirs,
       Path <- rebar_utils:find_files(Dir, ".backup$")
  ],
  ok.

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

-spec is_dep_name_fun(binary()) -> fun((_) -> boolean()).
is_dep_name_fun(Name) ->
  fun(Dep) -> Name =:= rebar_app_info:name(Dep) end.

-spec is_not_dep_name_fun(binary()) -> fun((_) -> boolean()).
is_not_dep_name_fun(Name) ->
  IsDepName = is_dep_name_fun(Name),
  fun(Dep) -> not IsDepName(Dep) end.

-spec compile(rebar_app_info:t(), [any()]) -> boolean().
compile(AppInfo, Config0) ->
  Graph  = load_graph(AppInfo),
  try find_files_to_compile(AppInfo, Graph) of
    [] ->
      false;
    SrcFiles ->
      rebar_api:info("Clojerl Compiling ~s", [rebar_app_info:name(AppInfo)]),
      EbinDir = rebar_app_info:ebin_dir(AppInfo),
      Config1 = Config0#{ebin_dir => EbinDir, graph => Graph},
      [ compile_clje(Src, Config1#{src_dir => SrcDir})
        || {SrcDir, Src} <- SrcFiles
      ],
      store_graph(AppInfo, Graph),
      true
  after
    digraph:delete(Graph)
  end.

-spec compile_clje(file:name(), [any()]) -> ok.
compile_clje(Src, Config) ->
  rebar_api:debug("Compiling ~s...", [Src]),

  SrcDir   = list_to_binary(maps:get(src_dir, Config)),
  EbinDir  = list_to_binary(maps:get(ebin_dir, Config)),
  ProtoDir = list_to_binary(maps:get(protocols_dir, Config)),
  Graph    = maps:get(graph, Config),

  Bindings  = #{ <<"#'clojure.core/*compile-files*">>          => true
               , <<"#'clojure.core/*compile-path*">>           => EbinDir
               , <<"#'clojure.core/*compile-protocols-path*">> => ProtoDir
               },
  try
    ok      = 'clojerl.Var':push_bindings(Bindings),
    Targets = clj_compiler:compile_file(list_to_binary(Src)),
    update_graph(remove_src_dir(Src, SrcDir, list), Targets, Graph)
  catch
    _:Reason ->
      Stacktrace = erlang:get_stacktrace(),
      rebar_api:debug("Stacktrace:~n~s", [clj_utils:stacktrace(Stacktrace)]),
      rebar_api:abort( "Error while compiling ~s: ~s"
                     , [Src, clj_rt:str(Reason)]
                     )
  after
    ok = 'clojerl.Var':pop_bindings()
  end.

%% =============================================================================
%% Graph management

%% TODO: delete cljinfo when cleaning

-spec load_graph(rebar_app_info:t()) -> digraph:graph().
load_graph(AppInfo) ->
  Graph   = digraph:new([acyclic]),
  CljInfo = cljinfo_file(AppInfo),
  case file:read_file(CljInfo) of
    {ok, Data}  ->
      #{edges := Edges, vertices := Vertices} = binary_to_term(Data),
      [digraph:add_vertex(Graph, V, Label) || {V, Label}     <- Vertices],
      [digraph:add_edge(Graph, V1, V2)     || {_, V1, V2, _} <- Edges];
    {error, _} -> ok
  end,
  Graph.

-spec update_graph(binary(), [binary()], digraph:graph()) -> ok.
update_graph(_Source, [], _Graph)  -> ok;
update_graph(Source, [Target | Targets], Graph) ->
  TargetFilename = rebar_utils:to_list(filename:basename(Target)),

  digraph:add_vertex(Graph, Source),
  digraph:add_vertex(Graph, TargetFilename),
  digraph:add_edge(Graph, Source, TargetFilename),

  rebar_api:debug("~s -> ~s", [Source, TargetFilename]),

  update_graph(Source, Targets, Graph).

-spec store_graph(rebar_app_info:t(), digraph:graph()) -> ok.
store_graph(AppInfo, Graph) ->
  Vertices = [digraph:vertex(Graph, V) || V <- digraph:vertices(Graph)],
  Edges    = [digraph:edge(Graph, E)   || E <- digraph:edges(Graph)],
  Map      = #{ edges    => Edges
              , vertices => Vertices
              },
  CljInfo  = cljinfo_file(AppInfo),
  ok       = filelib:ensure_dir(CljInfo),
  Data     = term_to_binary(Map, [{compressed, 2}]),
  file:write_file(CljInfo, Data).

-spec cljinfo_file(rebar_app_info:t()) -> file:name().
cljinfo_file(AppInfo) ->
  OutDir  = rebar_app_info:out_dir(AppInfo),
  filename:join(rebar_dir:local_cache_dir(OutDir), ?CLJINFO_FILE).

%% =============================================================================
%% Find files to compile

-spec find_files_to_compile(rebar_app_info:t(), digraph:graph()) ->
  [{file:name(), file:name()}].
find_files_to_compile(AppInfo, Graph) ->
  OutDir      = rebar_app_info:out_dir(AppInfo),
  EbinDir     = rebar_app_info:ebin_dir(AppInfo),
  CljeSrcDirs = rebar_app_info:get(AppInfo, clje_src_dirs, ?DEFAULT_SRC_DIRS),

  SrcDirPaths = [filename:join(OutDir, Dir) || Dir <- CljeSrcDirs],
  ok          = code:add_pathsa(SrcDirPaths),
  Fun         = fun(SrcDir) ->
                    find_files_to_compile(SrcDir, EbinDir, Graph)
                end,

  lists:flatmap(Fun, SrcDirPaths).

-spec find_files_to_compile(file:name(), file:name(), digraph:graph()) ->
  [{file:name(), file:name()}].
find_files_to_compile(SrcDir, EbinDir, Graph) ->
  SrcFiles = rebar_utils:find_files(SrcDir, "clj[ce]"),
  [{SrcDir, Source} || Source <- SrcFiles,
                       should_compile_file(Source, SrcDir, EbinDir, Graph)
  ].

-spec should_compile_file( file:name()
                         , file:name()
                         , file:name()
                         , digraph:graph()
                         ) -> boolean().
should_compile_file(Src, SrcDir, EbinDir, Graph) ->
  MainTarget     = target_file(Src, SrcDir, EbinDir),
  SourceFilename = remove_src_dir(Src, SrcDir, list),
  OtherTargets   =
    [ filename:join(EbinDir, TargetFilename)
      || TargetFilename <- digraph:out_neighbours(Graph, SourceFilename)
    ],
  Targets        = [MainTarget | OtherTargets],
  %% rebar_api:debug("Should compile ~s given ~p", [SourceFilename, Targets]),
  lists:any(fun(Target) -> should_compile(Target, Src) end, Targets).

-spec target_file(file:name(), file:name(), file:name()) -> binary().
target_file(Src, SrcDir, OutDir) ->
  Target1 = remove_src_dir(Src, SrcDir),
  Target2 = clj_utils:resource_to_ns(Target1),
  Target3 = filename:basename(Target2, filename:extension(Src)),
  Target4 = [OutDir, "/", Target3, ".beam"],
  iolist_to_binary(Target4).

-spec should_compile(binary(), file:name()) -> boolean().
should_compile(Target, Source) ->
  not filelib:is_file(Target)
    orelse filelib:last_modified(Target) < filelib:last_modified(Source).

-spec remove_src_dir(file:name(), file:name()) -> file:name().
remove_src_dir(Src, SrcDir) ->
  remove_src_dir(Src, SrcDir, binary).

-spec remove_src_dir(file:name(), file:name(), list | binary) -> file:name().
remove_src_dir(Src, SrcDir, Return) ->
  re:replace(Src, ["^", SrcDir, "/"], "", [global, {return, Return}]).
