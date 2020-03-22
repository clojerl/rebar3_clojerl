-module(rebar3_clojerl_prv_repl).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, repl).
-define(NAMESPACE, clojerl).
-define(DEPS, [compile]).

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
             ],
  Provider = providers:create([ {namespace,  ?NAMESPACE}
                              , {name,       ?PROVIDER}
                              , {module,     ?MODULE}
                              , {bare,       true}
                              , {deps,       ?DEPS}
                              , { example
                                , "rebar3 clojerl repl "
                                  "--sname foo --apps bar,baz"
                                }
                              , {opts,       Opts}
                              , {short_desc, "Start a clojerl repl"}
                              , {desc,       "Start a clojerl repl"}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
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
  Bindings  = #{<<"#'clojure.core/*compile-files*">> => false},

  DepsPaths = rebar_state:code_paths(State, all_deps),
  code:add_pathsa(DepsPaths),

  {Opts, _} = rebar_state:command_parsed_args(State),
  ok        = maybe_start_apps(Opts),
  ok        = rebar3_clojerl_utils:maybe_set_sname(Opts),

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
      Apps = [list_to_atom(X) || X <- string:tokens(AppsStr, ",")],
      [application:ensure_all_started(Name) || Name <- Apps],
      ok
  end.
