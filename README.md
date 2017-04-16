clojerl-compiler
=====

Compile clojerl projects

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { clojerl-compiler, ".*", {git, "git@host:user/clojerl-compiler.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 clojerl-compiler
    ===> Fetching clojerl-compiler
    ===> Compiling clojerl-compiler
    <Plugin Output>
