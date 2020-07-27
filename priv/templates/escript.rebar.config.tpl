{erl_opts, [debug_info]}.

{deps, [{clojerl, "0.7.0"}]}.
{plugins, [{rebar3_clojerl, "0.8.3"}]}.

{escript_incl_apps, [{{name}}]}.
{escript_main_app, {{name}}}.
{escript_name, {{name}}}.
{escript_emu_args, "%%! +sbtu +A1\n"}.
