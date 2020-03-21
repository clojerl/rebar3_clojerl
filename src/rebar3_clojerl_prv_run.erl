-module(rebar3_clojerl_prv_run).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, run).
-define(NAMESPACE, clojerl).
-define(DEPS, [compile]).

-type opts() :: [{atom(), any()}].

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Opts     = [ { main, $m, "main", string
               , "Provide a fully qualified function, or a namespace "
                 "that contains a -main function"
               }
             ],
  ShortDescription = "Run the project's -main function.",
  Description =
    "Calls the -main function in the namespace specified in 'clje_main' in\n"
    "rebar.config, which can be overriden using the --main option. Arguments \n"
    "to the function can be provided after --.",
  Provider = providers:create([ {namespace,  ?NAMESPACE}
                              , {name,       ?PROVIDER}
                              , {module,     ?MODULE}
                              , {bare,       true}
                              , {deps,       ?DEPS}
                              , { example
                                , "rebar3 clojerl run -m foo/main -- 1 2"
                                }
                              , {opts,       Opts}
                              , {short_desc, ShortDescription}
                              , {desc,       Description}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  run(State),
  {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec run(rebar_state:t()) -> ok.
run(State) ->
  DepsPaths = rebar_state:code_paths(State, all_deps),
  code:add_pathsa(DepsPaths),

  %% Resolve module and functions
  {Opts, _} = rebar_state:command_parsed_args(State),
  Apps = rebar_state:project_apps(State),
  {M, F} = resolve_main(Apps, Opts),

  %% Resolve arguments
  RawArgs = rebar_state:command_args(State),
  Args = resolve_args(RawArgs),

  apply(M, F, Args).

-spec resolve_main([rebar_app_info:t()], opts()) -> ok.
resolve_main(Apps, Opts) ->
  CljeMain = find_clje_main(Apps),
  case proplists:get_value(main, Opts, CljeMain) of
    undefined ->
      rebar_api:abort("No main function or namespace specified", []);
    MainStr ->
      case string:tokens(MainStr, "/") of
        [Namespace] ->
          {list_to_atom(Namespace), '-main'};
        [Namespace, Function] ->
          {list_to_atom(Namespace), list_to_atom(Function)}
      end
  end.

-spec find_clje_main([rebar_app_info:t()]) -> string().
find_clje_main(Apps) ->
  Mains = [rebar_app_info:get(App, clje_main, undefined) || App <- Apps],
  case [X || X <- Mains, X =/= undefined] of
    []  -> undefined;
    [X] -> rebar_utils:to_list(X);
    List ->
      rebar_api:abort("More than one 'clje_main' found in the "
                      "project applications: ~p ", [List])
  end.

-spec resolve_args([string()]) -> [binary()].
resolve_args(RawArgs) ->
  case lists:dropwhile(fun(X) -> X =/= "--" end, RawArgs) of
    [] -> [];
    [_ | Args] ->
      [rebar_utils:to_binary(X) || X <- Args]
  end.
