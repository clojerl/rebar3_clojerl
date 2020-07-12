rebar3_clojerl
=====

[![Hex.pm](https://img.shields.io/hexpm/v/rebar3_clojerl.svg)](https://hex.pm/packages/rebar3_clojerl)

Compile clojerl projects

## Use

Add the plugin to your `rebar.config` (along with `clojerl` as a
dependency):

    {deps, [clojerl]}.
    {plugins, [rebar3_clojerl]}.

Then just call the plugin directly in an existing application:

    $ rebar3 clojerl compile
    ===> Fetching rebar3_clojerl
    ===> Compiling rebar3_clojerl

To get a list of all available tasks for the plugin run:

```
$ rebar3 help clojerl
===> Compiling rebar3_clojerl

clojerl <task>:
  compile           Compile clojerl project
  escriptize        Generate escript archive.
  release           Build release of Clojerl project.
  repl              Start a clojerl repl
  run               Run the project's -main function.
  test              Test clojerl project
```

For each of these you can then get more details and available options
by running for example:

    $ rebar3 help clojerl repl
    Start a clojerl repl
	Usage: rebar3 clojerl repl [--apps <apps>] [--sname <sname>]

	  --apps   List of applications that should be started separated by commas
	           (e.g. --apps app1,app2,app3).
	  --sname  Erlang node name.

### rebar.config options

These are the available options:

| Name                 | Description                                                                             |
|----------------------|-----------------------------------------------------------------------------------------|
| `clje_src_dirs`      | List of directories where Clojerl source is located (default: `["src"]`).               |
| `clje_test_dirs`     | List of directories where Clojerl tests are located (default: `["test"]`).              |
| `clje_compile_first` | List of files that should be compiled first (default: `[]`).                            |
| `clje_exclude`       | List of files that should be excluded from compilation (default: `[]`).                 |
| `clje_main`          | String specifying either a fully qualified function or a namespace (e.g. `"foo/main"`). |

The first four are helpful for handling custom directory structure
and/or compilation behaviour.

For example, if the Clojerl code was in `src/clj` instead of just
`src`, the `rebar.config` should include the following entry:

```
{clje_src_dirs, ["src/clj"]}.
```

And if we wanted to have the file for namespace `foo.bar` compiled
first we would also include the entry:

```
{clj_compile_first, ["foo/bar.clje"]}.
```

## Plugin Development

Run the following commands to checkout the repository and build the
plugin:

    git checkout https://github.com/clojerl/rebar3_clojerl
    cd rebar3_clojerl
    rebar3 compile

### Install globally

`rebar3` also allows you to install [plugins globally][rebar3-plugins]
through its configuration file `~/.config/rebar3/rebar.config`.

By adding the following entry in the global `rebar.config` you will be
able to use the plugin to build your project or create a new one with
the available [template](#template).

    {plugins, [rebar3_clojerl]}.

## Templates

When the plugin is [installed globally](#install-globally) you can use
any of the available templates:

* `clojerl_app`: create a Clojerl OTP application.
* `clojerl_escript`: create a Clojerl escript.
* `clojerl_lib`: create a Clojerl library.
* `clojerl_release`: create a Clojerl release.

For example:

    rebar3 new clojerl_app awesome

### Building the generated application

Running `rebar3 clojerl compile` will build the application.

### Application name

Because of how Clojerl (and Clojure) processes dashes for namespace
names, and current limitations of the templating mechanism, you can't
include dashes in your application's name. It's sad, I know. :(

[rebar3-plugins]: https://www.rebar3.org/docs/using-available-plugins
[rebar3-provider-hooks]: https://www.rebar3.org/docs/configuration#section-provider-hooks
