{erl_opts, [debug_info]}.

{ deps
, [ {clojerl, {git, "https://github.com/clojerl/clojerl.git", {branch, "0.4.0"}}}
  ]
}.

{ provider_hooks
, [ {post, [{compile, {clojerl, compile}}]}
  ]
}.
