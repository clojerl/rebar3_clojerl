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
      test           Test clojerl project

For each of these you can then get more details and available options by running
for example:

    $ rebar3 help clojerl repl
    Start a clojerl repl
	Usage: rebar3 clojerl repl [--apps <apps>] [--sname <sname>]

	  --apps   List of applications that should be started separated by commas
	           (e.g. --apps app1,app2,app3).
	  --sname  Erlang node name.

### rebar.config options

The following options are helpful for handling custom directory structure and/or
compilation behaviour:

- `clje_src_dirs`: List of directories where Clojerl source is located (default: `["src"]`).
- `clje_test_dirs`: List of directories where Clojerl tests are located (default: `["test"]`).
- `clje_compile_first`: List of files that should be compiled first (default: `[]`).
- `clje_exclude`: List of files that should be excluded from compilation (default: `[]`).

For example, if the Clojerl code was in `src/clj` instead of just `src`, the
`rebar.config` should include the following entry:

```
{clje_src_dirs, ["src/clj"]}.
```

And if we wanted to have the file for namespace `foo.bar` compiled first we would
also include the entry:

```
{clj_compile_first, ["foo/bar.clje"]}.
```

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

### Building the generated application

There is a [provider_hook][rebar3-provider-hooks] in the generated application that
will run `rebar3 clojerl compile` after running `rebar3 compile`. Therefore the current
way of building a Clojerl application is by running `rebar3 compile` and **not**
`rebar3 clojerl compile`.

### Application name

Because of how Clojerl (and Clojure) processes dashes for namespace names,
and current limitations of the templating mechanism, you can't include dashes in your
application's name. It's sad, I know. :(

[rebar3-plugins]: https://www.rebar3.org/docs/using-available-plugins
[rebar3-provider-hooks]: https://www.rebar3.org/docs/configuration#section-provider-hooks
