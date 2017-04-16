-module('clojerl-compiler').

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = 'clojerl-compiler_prv':init(State),
    {ok, State1}.
