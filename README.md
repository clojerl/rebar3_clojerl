rebar3_clojerl
=====

Compile clojerl projects

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    { plugins
    , [ {rebar3_clojerl, ".*", {git, "https://github.com/clojerl/rebar3_clojerl", {tag, "0.1.0"}}}
      ]
    }.

Then just call the plugin directly in an existing application:

    $ rebar3 clojerl compile
    ===> Fetching rebar3_clojerl
    ===> Compiling rebar3_clojerl

To get a list of all available tasks for the plugin run:

    $ rebar3 help clojerl
    ===> Compiling rebar3_clojerl

    clojerl <task>:
      compile        Compile clojerl project
      repl           Start a clojerl repl
