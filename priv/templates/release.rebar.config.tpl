{erl_opts, [debug_info]}.

{deps, [{clojerl, "0.7.1"}]}.
{plugins, [{rebar3_clojerl, "0.8.6"}]}.

{relx, [ { release
         , {{{name}}, "0.1.0"}
         , [ {{name}}
           , sasl
           ]
         },

        {sys_config, "./config/sys.config"},
        {vm_args, "./config/vm.args"},

        {dev_mode, true},
        {include_erts, false},

        {extended_start_script, true}]
}.

{ profiles
, [ { prod
    , [ {relx, [ {dev_mode, false}
               , {include_erts, true}
               ]
        }
      ]
    }
  ]
}.
