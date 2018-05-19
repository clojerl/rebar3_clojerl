-module(rebar3_clojerl).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State0) ->
  {ok, State1} = rebar3_clojerl_prv_compile:init(State0),
  {ok, State2} = rebar3_clojerl_prv_repl:init(State1),
  {ok, _     } = rebar3_clojerl_prv_release:init(State2).
