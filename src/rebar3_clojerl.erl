-module(rebar3_clojerl).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  {ok, State1}  = rebar3_clojerl_compile_prv:init(State),
  {ok, _State2} = rebar3_clojerl_repl_prv:init(State1).
