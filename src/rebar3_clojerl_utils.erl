-module(rebar3_clojerl_utils).

-include("rebar3_clojerl.hrl").

-export([ ensure_clojerl/0
        , all_apps/1
        , find_app/2
        , filter_app/2
        , maybe_set_sname/1
        , update_app_file/1
        ]).

-type opts() :: [{atom(), any()}].

-spec ensure_clojerl() -> ok.
ensure_clojerl() ->
  case code:ensure_loaded(clojerl) of
    {module, clojerl} ->
      ok = clojerl:start();
    {error, Reason} ->
      rebar_api:abort("Application Clojerl could not be started: ~p", [Reason])
  end.

%% @doc Updates the list of modules in the .app file for the specified
%% directory.
%%
%% The .app file will be update to include all modules in its
%% `modules' entry. The modules listed are resolved by looking for all
%% files with the extension `.beam' in `Dir'.
-spec update_app_file(file:name()) -> ok.
update_app_file(Dir) ->
  case rebar_utils:find_files(Dir, ".app$", false) of
    [AppFile] ->
      {ok, [{application, AppName, AppDetail0}]} = file:consult(AppFile),

      BeamPaths = rebar_utils:find_files(Dir, ".beam$", false),
      Modules   = [ list_to_atom(filename:basename(Path, ".beam"))
                    || Path <- BeamPaths
                  ],
      AppDetail1  = lists:keyreplace( modules
                                    , 1
                                    , AppDetail0
                                    , {modules, Modules}
                                    ),
      SpecBefore = io_lib:format("~p.\n", [{application, AppName, AppDetail0}]),
      SpecAfter = io_lib:format("~p.\n", [{application, AppName, AppDetail1}]),

      rebar_api:debug("Updating app file for ~p", [AppName]),
      rebar_api:debug("~p.app (BEFORE):~n~s", [AppName, SpecBefore]),
      rebar_api:debug("~p.app (AFTER):~n~s", [AppName, SpecAfter]),

      ok = rebar_file_utils:write_file_if_contents_differ( AppFile
                                                         , SpecAfter
                                                         , utf8
                                                         ),
      ok;
    [] -> ok
  end.

-spec all_apps(rebar_state:t()) -> [rebar_app_info:t()].
all_apps(State) ->
  lists:usort(rebar_state:all_deps(State)) ++ rebar_state:project_apps(State).

-spec find_app(rebar_state:t(), binary()) -> notfound | {ok, any()}.
find_app(Apps, Name) ->
  case lists:filter(is_app_name_fun(Name), Apps) of
    [] -> notfound;
    [DepInfo] -> {ok, DepInfo}
  end.

-spec filter_app([rebar_app_info:t()], binary()) -> [rebar_app_info:t()].
filter_app(Apps, Name) ->
  lists:filter(is_not_app_name_fun(Name), Apps).

-spec is_app_name_fun(binary()) -> fun((_) -> boolean()).
is_app_name_fun(Name) ->
  fun(Dep) -> Name =:= rebar_app_info:name(Dep) end.

-spec is_not_app_name_fun(binary()) -> fun((_) -> boolean()).
is_not_app_name_fun(Name) ->
  IsDepName = is_app_name_fun(Name),
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
