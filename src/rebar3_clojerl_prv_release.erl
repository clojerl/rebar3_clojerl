-module(rebar3_clojerl_prv_release).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, release).
-define(NAMESPACE, clojerl).
-define(NAMESPACE_PROVIDER, {?NAMESPACE, ?PROVIDER}).
-define(DEPS, [{?NAMESPACE, compile}]).

%% =============================================================================
%% Public API
%% =============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Provider = providers:create([ {namespace,  ?NAMESPACE}
                              , {name, ?PROVIDER}
                              , {module, ?MODULE}
                              , {bare, true}
                              , {deps, ?DEPS}
                              , {example, "rebar3 release"}
                              , { short_desc
                                , "Build a release for the Clojerl project."
                                }
                              , { desc
                                , "Build a release for the Clojerl project."
                                }
                              , {opts, relx:opt_spec_list()}
                              ]
                             ),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  rebar_relx:do(rlx_prv_release, "release", ?PROVIDER, State).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).
