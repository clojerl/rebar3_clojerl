-module(rebar3_clojerl_utils).

-include("rebar3_clojerl.hrl").

-export([ ensure_clojerl/1
        , all_apps/1
        , find_dep/2
        , filter_app/2
        , maybe_set_sname/1
        ]).

-type opts() :: [{atom(), any()}].

-spec ensure_clojerl(rebar_state:t()) -> ok.
ensure_clojerl(State) ->
  case find_dep(State, ?CLOJERL) of
    notfound ->
      rebar_api:abort("Clojerl was not found as a dependency", []);
    {ok, _} ->
      ok = clojerl:start()
  end.

-spec all_apps(rebar_state:t()) -> [rebar_app_info:t()].
all_apps(State) ->
  lists:usort(rebar_state:all_deps(State)) ++ rebar_state:project_apps(State).

-spec find_dep(rebar_state:t(), binary()) -> notfound | {ok, any()}.
find_dep(State, Name) ->
  Deps  = rebar_state:all_deps(State),
  case lists:filter(is_dep_name_fun(Name), Deps) of
    [] -> notfound;
    [DepInfo] -> {ok, DepInfo}
  end.

-spec filter_app([rebar_app_info:t()], binary()) -> notfound | {ok, any()}.
filter_app(Apps, Name) ->
  lists:filter(is_not_dep_name_fun(Name), Apps).

-spec is_dep_name_fun(binary()) -> fun((_) -> boolean()).
is_dep_name_fun(Name) ->
  fun(Dep) -> Name =:= rebar_app_info:name(Dep) end.

-spec is_not_dep_name_fun(binary()) -> fun((_) -> boolean()).
is_not_dep_name_fun(Name) ->
  IsDepName = is_dep_name_fun(Name),
  fun(Dep) -> not IsDepName(Dep) end.

-spec maybe_set_sname(opts()) -> ok.
maybe_set_sname(Opts) ->
  case proplists:get_value(sname, Opts, undefined) of
    undefined -> ok;
    SNameStr ->
      SName   = list_to_atom(SNameStr),
      case net_kernel:start([SName, shortnames]) of
        {ok, _} -> ok;
        {error, Reason} ->
          rebar_api:warn( "Couldn't start distribution and assign name ~p: ~p"
                        , [SName, Reason]
                        )
      end,
      ok
  end.
