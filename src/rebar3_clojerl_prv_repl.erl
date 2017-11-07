-module(rebar3_clojerl_repl_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, repl).
-define(NAMESPACE, clojerl).
-define(DEPS, [compile]).

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
                              , {example,    "rebar3 clojerl repl"}
                              , {opts,       []}
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
  Bindings = #{<<"#'clojure.core/*compile-files*">> => false},

  DepsPaths = rebar_state:code_paths(State, all_deps),
  code:add_pathsa(DepsPaths),

  try
    ok = 'clojerl.Var':push_bindings(Bindings),
    'clojure.main':main([<<"-r">>])
  after
    ok = 'clojerl.Var':pop_bindings()
  end.
