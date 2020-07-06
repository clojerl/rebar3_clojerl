-module(rebar3_clojerl).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Commands = [ fun rebar3_clojerl_prv_compile:init/1
             , fun rebar3_clojerl_prv_escriptize:init/1
             , fun rebar3_clojerl_prv_repl:init/1
             , fun rebar3_clojerl_prv_run:init/1
             , fun rebar3_clojerl_prv_test:init/1
             ],
  FoldFun  = fun(F, {ok, StateAcc}) -> F(StateAcc) end,
  lists:foldl(FoldFun, {ok, State}, Commands).
