{erl_opts, [debug_info]}.

{deps, [{clojerl, "0.5.0"}]}.

{ provider_hooks
, [ {post, [{compile, {clojerl, compile}}]}
  ]
}.
