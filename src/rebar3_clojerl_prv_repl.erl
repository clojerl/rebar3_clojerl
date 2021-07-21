-module(rebar3_clojerl_prv_repl).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, repl).
-define(NAMESPACE, clojerl).
-define(DEPS, [{?NAMESPACE, compile}]).

%% Redefining configuration for these apps can lead to errors.
-define(APP_BLACKLIST, [inets, stdlib, kernel, rebar]).

-type opts() :: [{atom(), any()}].

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Opts     = [ { apps, undefined, "apps", string
               , "List of applications that should be started "
                 "separated by commas (e.g. --apps app1,app2,app3)."
               }
             , { sname, undefined, "sname", string
               , "Erlang node name."
               }
             , { config, undefined, "config", string
               , "Path to the config file to load."
               }
             ],
  Provider = providers:create([ {namespace,  ?NAMESPACE}
                              , {name,       ?PROVIDER}
                              , {module,     ?MODULE}
                              , {bare,       true}
                              , {deps,       ?DEPS}
                              , { example
                                , "rebar3 clojerl repl "
                                  "--sname foo --apps bar,baz --config sys.config"
                                }
                              , {opts,       Opts}
                              , {short_desc, "Start a clojerl repl"}
                              , {desc,       "Start a clojerl repl"}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  rebar3_clojerl_utils:ensure_clojerl(),
  repl(State),
  {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

%% =============================================================================
%% Internal functions
%% =============================================================================

-spec repl(rebar_state:t()) -> ok.
repl(State) ->
  maybe_restart_clojerl(),
  Bindings    = #{<<"#'clojure.core/*compile-files*">> => false},
  DepsPaths   = rebar_state:code_paths(State, all_deps),
  {Opts, _}   = rebar_state:command_parsed_args(State),
  AppsToStart = read_apps(proplists:get_value(apps, Opts, "")),

  code:add_pathsa(DepsPaths),

  ok          = load_apps(AppsToStart),
  ok          = maybe_apply_config(AppsToStart, State, Opts),
  ok          = rebar3_clojerl_utils:maybe_set_sname(Opts),
  ok          = maybe_start_apps(Opts),

  try
    ok = 'clojerl.Var':push_bindings(Bindings),
    'clojure.main':main([<<"-r">>])
  after
    ok = 'clojerl.Var':pop_bindings()
  end.

%% @doc Restarts the clojerl application when clje.user doesn't
%% refer vars from clojure.core.
-spec maybe_restart_clojerl() -> ok.
maybe_restart_clojerl() ->
  CljeUserSym = clj_rt:symbol(<<"clje.user">>),
  CljeUserNs = 'clojerl.Namespace':find(CljeUserSym),
  SeqSym = clj_rt:symbol(<<"seq">>),
  case 'clojerl.Namespace':find_var(CljeUserNs, SeqSym) of
    undefined ->
      application:stop(clojerl),
      clojerl:start();
    _ -> ok
  end.

-spec maybe_start_apps(opts()) -> ok.
maybe_start_apps(Opts) ->
  case proplists:get_value(apps, Opts, undefined) of
    undefined -> ok;
    AppsStr ->
      Apps = read_apps(AppsStr),
      [application:ensure_all_started(Name) || Name <- Apps],
      ok
  end.

-spec load_apps([atom()]) -> ok.
load_apps(Apps) ->
  [ case application:load(App) of
      ok ->
        {ok, Ks} = application:get_all_key(App),
        %% Load dependencies as well
        load_apps(proplists:get_value(applications, Ks));
      _ -> error
    end
    || App <- Apps
  ],
  ok.

-spec read_config(rebar_state:t(), opts()) -> [[tuple()]] | no_config.
read_config(State, Opts) ->
  case proplists:get_value(config, Opts, no_value) of
    no_value -> no_config;
    Filename when is_list(Filename) ->
      rebar_file_utils:consult_config(State, Filename)
  end.

-spec read_apps(string()) -> [atom()].
read_apps(AppsStr) ->
  [list_to_atom(X) || X <- string:tokens(AppsStr, ",")].

-spec maybe_apply_config([atom()], rebar_state:t(), opts()) -> ok.
maybe_apply_config(AppsToStart, State, Opts) ->
  case read_config(State, Opts) of
    no_config -> ok;
    ConfigList ->
      %% Copied from:
      %% https://github.com/erlang/rebar3/blob/master/src/rebar_prv_shell.erl
      Running = [App || {App, _, _} <- application:which_applications()],
      [ application:stop(App)
	|| Config   <- ConfigList,
	   {App, _} <- Config,
	   lists:member(App, Running),
	   lists:member(App, AppsToStart),
	   not lists:member(App, ?APP_BLACKLIST)
      ],
      _ = rebar_utils:reread_config(ConfigList, [update_logger]),
      ok
  end.
