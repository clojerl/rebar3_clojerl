-module(rebar3_clojerl_prv_compile).

-include("rebar3_clojerl.hrl").

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, clojerl).
-define(NAMESPACE_PROVIDER, {?NAMESPACE, ?PROVIDER}).
-define(DEPS, [{default, compile}]).

-type config() :: #{ ebin_dir      => file:name()
                   , protocols_dir => file:name()
                   , src_dir       => file:name()
                   , graph         => digraph:graph()
                   }.

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
  DepsPaths   = rebar_state:code_paths(State, all_deps),
  ok          = code:add_pathsa(DepsPaths),

  ProjectApps = rebar_state:project_apps(State),
  Deps0       = rebar_state:all_deps(State),
  {ok, Deps1} = rebar_digraph:compile_order(Deps0),

  AppsPaths   = [rebar_app_info:ebin_dir(AppInfo) || AppInfo <- ProjectApps],
  ok          = code:add_pathsa(AppsPaths),

  AllApps0    = Deps1 ++ ProjectApps,
  Config      = #{protocols_dir => protocols_dir(State)},

  %% Add the src directories for the all the applications
  [code:add_pathsa(clje_src_dirs(AppInfo)) || AppInfo <- AllApps0],

  try
    %% More than one application might modify existing protocols, so we
    %% restore the original backed-up protocol modules before compiling.
    restore_duplicates(DepsPaths),

    %% Run top level pre hooks
    Cwd       = rebar_state:dir(State),
    Providers = rebar_state:providers(State),
    rebar_hooks:run_all_hooks(Cwd, pre, ?NAMESPACE_PROVIDER, Providers, State),

    %% The Erlang modules for clojerl have been compiled at this point.
    %% Make sure the clojerl application is available and started.
    ok       = rebar3_clojerl_utils:ensure_clojerl(),

    AllApps1 = compile_clojerl(AllApps0, Config, Providers, State),
    [compile(AppInfo, Config, Providers, State) || AppInfo <- AllApps1],

    %% Run top level post hooks
    rebar_hooks:run_all_hooks(Cwd, post, ?NAMESPACE_PROVIDER, Providers, State)
  after
    backup_duplicates(DepsPaths, Config)
  end,

  {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec protocols_dir(rebar_state:t()) -> file:name().
protocols_dir(State) ->
  case rebar_state:project_apps(State) of
    %% Get the first application from this project.
    [AppInfo | _] ->
      ProtoDir = rebar_app_info:ebin_dir(AppInfo),
      rebar_api:debug("Protocols dir: ~s", [ProtoDir]),
      ProtoDir;
    %% When there are no applications use Clojerl's ebin
    [] ->
      Deps = rebar_state:all_deps(State),
      case rebar3_clojerl_utils:find_app(Deps, ?CLOJERL) of
        {ok, ClojerlApp} ->
          ProtoDir = rebar_app_info:ebin_dir(ClojerlApp),
          rebar_api:debug( "No project apps, using Clojerl's "
                           "ebin as protocol dir: ~s"
                         , [ProtoDir]
                         ),
          ProtoDir;
        notfound ->
          rebar_api:debug( "No project apps or Clojerl as a dep, "
                           "using './ebin' as protocol dir"
                         , []
                         ),
          "ebin"
      end
  end.

-spec compile_clojerl( [rebar_app_info:t()]
                     , config()
                     , providers:t()
                     , rebar_state:t()
                     ) ->
  [rebar_app_info:t()].
compile_clojerl(AllApps, Config, Providers, State) ->
  case rebar3_clojerl_utils:find_app(AllApps, ?CLOJERL) of
    notfound ->
      AllApps;
    {ok, ClojerlApp} ->
      compile(ClojerlApp, Config, Providers, State),
      AllApps -- [ClojerlApp]
  end.

-spec backup_duplicates([rebar_app_info:t()], config()) -> ok.
backup_duplicates(DepsDirs, Config) ->
  ProtoDir      = maps:get(protocols_dir, Config),
  BeamFilepaths = rebar_utils:find_files(ProtoDir, ".beam$"),
  BeamFilenames = [filename:basename(F) || F <- BeamFilepaths],

  Dirs = DepsDirs -- [ProtoDir],
  rebar_api:debug("Finding duplicates for:~n~p~nin~n~p", [BeamFilenames, Dirs]),
  [backup_duplicates_from_dir(BeamFilenames, Dir) || Dir <- Dirs],
  ok.

-spec backup_duplicates_from_dir([file:name()], file:name()) -> [file:name()].
backup_duplicates_from_dir(BeamFilenames, Dir) ->
  [ begin
      rebar_api:debug("Backup duplicate ~s", [F]),
      ok = file:rename(F, F ++ ".backup"),
      F
    end
    || F <- rebar_utils:find_files(Dir, ".beam$"),
       lists:member(filename:basename(F), BeamFilenames)
  ].

-spec restore_duplicates([file:name()]) -> ok.
restore_duplicates(Dirs) ->
  [ begin
      DestPath = filename:rootname(Path),
      ok       = file:rename(Path, DestPath),
      rebar_api:debug("Restored duplicate ~s", [DestPath])
    end
    || Dir  <- Dirs,
       Path <- rebar_utils:find_files(Dir, ".backup$")
  ],
  ok.

-spec compile(rebar_app_info:t(), config(), providers:t(), rebar_state:t()) ->
  boolean().
compile(AppInfo0, Config0, Providers, State) ->
  AppDir  = rebar_app_info:dir(AppInfo0),
  AppInfo = rebar_hooks:run_all_hooks( AppDir
                                     , pre
                                     , ?NAMESPACE_PROVIDER
                                     , Providers
                                     , AppInfo0
                                     , State
                                     ),

  Graph   = load_graph(AppInfo),
  Config1 = Config0#{graph => Graph},
  AppName = rebar_app_info:name(AppInfo),
  try find_files_to_compile(AppInfo) of
    [] ->
      rebar_api:debug("No files to compile for ~s", [AppName]),
      false;
    SrcFiles ->
      rebar_api:info("Clojerl Compiling ~s", [AppName]),
      rebar_api:debug("Candidate files to compile: ~p", [SrcFiles]),
      EbinDir  = rebar_app_info:ebin_dir(AppInfo),
      Config2 = Config1#{ebin_dir => EbinDir},
      [ compile_clje(Src, Config2#{src_dir => SrcDir})
        || {SrcDir, Src} <- SrcFiles,
           should_compile_file(Src, SrcDir, Graph)
      ],
      store_graph(AppInfo, Graph),
      rebar_hooks:run_all_hooks( AppDir
                               , post
                               , ?NAMESPACE_PROVIDER
                               , Providers
                               , AppInfo
                               , State
                               ),
      true
  after
    digraph:delete(Graph)
  end.

-spec compile_clje(file:name(), config()) -> ok.
compile_clje(Src, Config) ->
  io:format("===> Compiling ~s...~n", [Src]),

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
    FullSrc = filename:join(SrcDir, Src),
    Targets = clj_compiler:file(FullSrc),
    update_graph(Src, Targets, Graph)
  catch ?WITH_STACKTRACE(_, Reason, Stacktrace)
      rebar_api:abort( "Error while compiling ~s: ~s~n"
                       "Stacktrace:~n~s"
                     , [ Src
                       , clj_rt:str(Reason)
                       , [clj_utils:format_stacktrace(Stacktrace)]
                       ]
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

-spec update_graph(file:name(), [binary()], digraph:graph()) -> ok.
update_graph(_Source, [], _Graph)  -> ok;
update_graph(Source, [Target | Targets], Graph) ->
  TargetFilename = rebar_utils:to_list(filename:basename(Target)),

  digraph:add_vertex(Graph, Source),
  digraph:add_vertex(Graph, TargetFilename),
  digraph:add_edge(Graph, Source, TargetFilename),

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

-spec find_files_to_compile(rebar_app_info:t()) -> [{file:name(), file:name()}].
find_files_to_compile(AppInfo) ->
  CljeSrcPaths = clje_src_dirs(AppInfo),
  CljeFirst    = clje_compile_first(AppInfo),
  CljeExclude  = clje_exclude(AppInfo),

  AllFiles     = lists:flatmap(fun find_files/1, CljeSrcPaths),
  SortFun      = fun({_, X}, {_, Y}) ->
                     maps:get(X, CljeFirst, -1) > maps:get(Y, CljeFirst, -1)
                 end,

  FilesToCompile = [ X || {_, Src} = X <- lists:sort(SortFun, AllFiles),
                          not lists:member(Src, CljeExclude)
                   ],

  lists:sort(FilesToCompile).

-spec find_files(file:name()) -> [{file:name(), file:name()}].
find_files(SrcDir) ->
  SrcFiles = rebar_utils:find_files(SrcDir, ".clj[ce]"),
  [{SrcDir, remove_src_dir(Source, SrcDir)} || Source <- SrcFiles].

-spec should_compile_file( file:name()
                         , file:name()
                         , digraph:graph()
                         ) -> boolean().
should_compile_file(Src, SrcDir, Graph) ->
  %% Check if the target file is either in the ebin directory or the
  %% protocols directory.
  FullSrc = filename:join(SrcDir, Src),
  rebar_api:debug("Checking if ~s should be compiled...", [Src]),
  Fun = fun(Target) ->
            should_compile(FullSrc, Target, code:get_path())
        end,
  case digraph:out_neighbours(Graph, Src) of
    [] ->
      %% Assume the target from the filename
      %% when there are none in the graph
      Target = src_to_target(Src),
      Fun(Target);
    Targets ->
      lists:any(Fun, Targets)
  end.

-spec should_compile(file:name(), binary(), [file:name()]) -> boolean().
should_compile(_FullSource, _Target, []) ->
  true;
should_compile(FullSource, Target, [Dir | Dirs]) ->
  FullTarget = filename:join(Dir, Target),
  case should_compile(FullSource, FullTarget) of
    false ->
      rebar_api:debug("Should NOT be compiled because of ~s.", [FullTarget]),
      false;
    true ->
      should_compile(FullSource, Target, Dirs)
  end.

-spec should_compile(file:name(), binary()) -> boolean().
should_compile(Source, Target) ->
  not filelib:is_file(Target)
    orelse filelib:last_modified(Target) < filelib:last_modified(Source)
    orelse not is_clojerl_compiled(Target).

-spec remove_src_dir(file:name(), file:name()) -> file:name().
remove_src_dir(Src, SrcDir) ->
  re:replace(Src, ["^", SrcDir, "/"], "", [global, {return, list}]).

-spec clje_compile_first(rebar_app_info:t()) -> #{list() => non_neg_integer()}.
clje_compile_first(AppInfo) ->
  CljeFirst = rebar_app_info:get(AppInfo, clje_compile_first, []),
  Fun = fun (X, {Acc, N}) ->
            {Acc#{X => N}, N - 1}
        end,
  {Positions, _} = lists:foldl(Fun, {#{}, length(CljeFirst)}, CljeFirst),
  Positions.

-spec src_to_target(file:name()) -> file:filename_all().
src_to_target(Src) ->
  SrcBin    = iolist_to_binary(Src),
  Filename0 = clj_utils:resource_to_ns(SrcBin),
  Filename1 = filename:rootname(Filename0),
  <<Filename1/binary, ".beam">>.

-spec is_clojerl_compiled(file:name()) -> boolean().
is_clojerl_compiled(Path) ->
  PathStr    = rebar_utils:to_list(Path),
  CoreChunk  = clj_utils:core_chunk(),
  ChunkNames = [CoreChunk],
  ChunkOpts  = [allow_missing_chunks],

  {ok, {_, Chunks}} = beam_lib:chunks(PathStr, ChunkNames, ChunkOpts),
  proplists:get_value(CoreChunk, Chunks) =/= missing_chunk.

-spec clje_src_dirs(rebar_app_info:t()) -> [string()].
clje_src_dirs(AppInfo) ->
  OutDir      = rebar_app_info:out_dir(AppInfo),
  CljeSrcDirs = rebar_app_info:get(AppInfo, clje_src_dirs, ?DEFAULT_SRC_DIRS),
  [filename:join(OutDir, Dir) || Dir <- CljeSrcDirs].

-spec clje_exclude(rebar_app_info:t()) -> [string()].
clje_exclude(AppInfo) ->
  rebar_app_info:get(AppInfo, clje_exclude, []).
