-module(rebar3_clojerl).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  {ok, State1}  = rebar3_clojerl_prv_compile:init(State),
  {ok, _State2} = rebar3_clojerl_prv_repl:init(State1).
