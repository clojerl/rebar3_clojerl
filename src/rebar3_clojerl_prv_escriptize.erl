-module(rebar3_clojerl_prv_escriptize).

-include("rebar3_clojerl.hrl").

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, escriptize).
-define(NAMESPACE, clojerl).
-define(DEPS, [{?NAMESPACE, compile}]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Description =
    "Generate a Clojerl escript executable containing the project's and its \n"
    "dependencies' BEAM files. This command uses rebar3 code under the hood \n"
    "so it supports the same `rebar.config` configuration values as the \n"
    "default `escriptize`.",
  Provider = providers:create([ {namespace,  ?NAMESPACE}
                              , {name,       ?PROVIDER}
                              , {module,     ?MODULE}
                              , {bare,       true}
                              , {deps,       ?DEPS}
                              , {example,    "rebar3 clojerl escriptize"}
                              , {opts,       []}
                              , {short_desc, "Generate escript archive."}
                              , {desc,       Description}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  rebar_prv_escriptize:do(State).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).
