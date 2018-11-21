rebar3_clojerl
=====

Compile clojerl projects

## Build

    $ rebar3 compile

## Use

Add the plugin to your rebar config:

    { plugins
    , [ {rebar3_clojerl, ".*", {git, "https://github.com/clojerl/rebar3_clojerl", {tag, "0.6.5"}}}
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


### Install globally

`rebar3` also allows you to install [plugins globally][rebar3-plugins] through its
configuration file `~/.config/rebar3/rebar.config`.

By adding the following entry in the `plugins` section of the global `rebar.config`
you will be able to use the plugin to build your project or create a new one through
with the available [template](#template).

    {rebar3_clojerl, ".*", {git, "https://github.com/clojerl/rebar3_clojerl", {tag, "0.6.6"}}}

## Template

When the plugin is [installed globally](#install-globally) you can use its template
to create a new Clojerl application.

    rebar3 new clojerl_app awesome

Because of how Clojerl (and Clojure) processes dashes for namespace names, and
current limitations of the templating mechanism, you can't include dashes in your
application's name. It's sad, I know. :(

[rebar3-plugins]: https://www.rebar3.org/docs/using-available-plugins
