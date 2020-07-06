{ application, '{{name}}'
, [ {vsn, git}
  , {description, "{{description}}"}
  , {modules, []}
  , { applications
    , [ stdlib
      , kernel
      , clojerl
      ]
    }
  ]
}.
