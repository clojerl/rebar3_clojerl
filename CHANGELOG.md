# Changelog

## [0.8.3](https://github.com/clojerl/rebar3_clojerl/tree/0.8.3)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.8.2...0.8.3)

**Implemented enhancements:**

- Bump versions in templates [\#95](https://github.com/clojerl/rebar3_clojerl/issues/95)

**Closed issues:**

- Generate documentation with edoc [\#93](https://github.com/clojerl/rebar3_clojerl/issues/93)

**Merged pull requests:**

- \[\#95\] Bump versions [\#96](https://github.com/clojerl/rebar3_clojerl/pull/96) ([jfacorro](https://github.com/jfacorro))
- \[\#93\] Generate edoc [\#94](https://github.com/clojerl/rebar3_clojerl/pull/94) ([jfacorro](https://github.com/jfacorro))

## [0.8.2](https://github.com/clojerl/rebar3_clojerl/tree/0.8.2) (2020-07-27)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.8.1...0.8.2)

**Implemented enhancements:**

- Make {clojerl, compile} depend on {default, compile} [\#91](https://github.com/clojerl/rebar3_clojerl/issues/91)

**Closed issues:**

- Enable starting a REPL outside of a project context [\#88](https://github.com/clojerl/rebar3_clojerl/issues/88)
- Possible reader issues when running the Clojerl REPL in a release? [\#86](https://github.com/clojerl/rebar3_clojerl/issues/86)

**Merged pull requests:**

- \[\#91\] Make 'clojerl compile' depend on default 'compile' [\#92](https://github.com/clojerl/rebar3_clojerl/pull/92) ([jfacorro](https://github.com/jfacorro))
- \[\#88\] Improve error messaging and handling when clojerl is not available [\#89](https://github.com/clojerl/rebar3_clojerl/pull/89) ([jfacorro](https://github.com/jfacorro))

## [0.8.1](https://github.com/clojerl/rebar3_clojerl/tree/0.8.1) (2020-07-22)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.8.0...0.8.1)

**Closed issues:**

- Issues running a Clojerl release [\#83](https://github.com/clojerl/rebar3_clojerl/issues/83)

**Merged pull requests:**

- Added support for creating library projects. [\#87](https://github.com/clojerl/rebar3_clojerl/pull/87) ([oubiwann](https://github.com/oubiwann))
- \[\#83\] Update .app files with modules before creating a release [\#84](https://github.com/clojerl/rebar3_clojerl/pull/84) ([jfacorro](https://github.com/jfacorro))
- Tweaked release plugin to mirror Erlang more closely. [\#82](https://github.com/clojerl/rebar3_clojerl/pull/82) ([oubiwann](https://github.com/oubiwann))

## [0.8.0](https://github.com/clojerl/rebar3_clojerl/tree/0.8.0) (2020-07-07)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.7.0...0.8.0)

**Implemented enhancements:**

- Include stacktrace on error  [\#75](https://github.com/clojerl/rebar3_clojerl/issues/75)
- Add escriptize command [\#67](https://github.com/clojerl/rebar3_clojerl/issues/67)
- Create template for creating escripts [\#63](https://github.com/clojerl/rebar3_clojerl/issues/63)

**Fixed bugs:**

- Run command fails with more than one argument [\#73](https://github.com/clojerl/rebar3_clojerl/issues/73)
- repl command crashes on first compilation on OTP 19 [\#72](https://github.com/clojerl/rebar3_clojerl/issues/72)

**Closed issues:**

- Release support? [\#79](https://github.com/clojerl/rebar3_clojerl/issues/79)
- Generate .app.src from project.clj [\#10](https://github.com/clojerl/rebar3_clojerl/issues/10)

**Merged pull requests:**

- Prepare release 0.8.0 [\#81](https://github.com/clojerl/rebar3_clojerl/pull/81) ([jfacorro](https://github.com/jfacorro))
- \[\#79\] Release template and command [\#80](https://github.com/clojerl/rebar3_clojerl/pull/80) ([jfacorro](https://github.com/jfacorro))
- \[\#63\] escript template [\#78](https://github.com/clojerl/rebar3_clojerl/pull/78) ([jfacorro](https://github.com/jfacorro))
- \[\#67\] Add escriptize command [\#77](https://github.com/clojerl/rebar3_clojerl/pull/77) ([jfacorro](https://github.com/jfacorro))
- \[\#75\] Include stacktrace on compile error [\#76](https://github.com/clojerl/rebar3_clojerl/pull/76) ([jfacorro](https://github.com/jfacorro))
- \[\#73\] Resolve var and then apply the arguments [\#74](https://github.com/clojerl/rebar3_clojerl/pull/74) ([jfacorro](https://github.com/jfacorro))

## [0.7.0](https://github.com/clojerl/rebar3_clojerl/tree/0.7.0) (2020-03-22)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.6.8...0.7.0)

**Fixed bugs:**

- Error generated when providing --var for running a test [\#60](https://github.com/clojerl/rebar3_clojerl/issues/60)

**Closed issues:**

- clje.user ns in REPL doesn't refer clojure.core when started from clean project  [\#69](https://github.com/clojerl/rebar3_clojerl/issues/69)
- Add run command [\#65](https://github.com/clojerl/rebar3_clojerl/issues/65)

**Merged pull requests:**

- Prepare release 0.7.0 [\#71](https://github.com/clojerl/rebar3_clojerl/pull/71) ([jfacorro](https://github.com/jfacorro))
- \[\#69\] Restart clojerl before starting the REPL when clje.user doesn't refer vars from clojure.core [\#70](https://github.com/clojerl/rebar3_clojerl/pull/70) ([jfacorro](https://github.com/jfacorro))
- \[\#65\] Add run command [\#66](https://github.com/clojerl/rebar3_clojerl/pull/66) ([jfacorro](https://github.com/jfacorro))
- Renamed compile\_file to file [\#64](https://github.com/clojerl/rebar3_clojerl/pull/64) ([jfacorro](https://github.com/jfacorro))
- \[\#60\] Also handle result from clojure.test/test-var [\#62](https://github.com/clojerl/rebar3_clojerl/pull/62) ([jfacorro](https://github.com/jfacorro))
- \[\#60\] Also handle result from clojure.test/test-var [\#61](https://github.com/clojerl/rebar3_clojerl/pull/61) ([jfacorro](https://github.com/jfacorro))

## [0.6.8](https://github.com/clojerl/rebar3_clojerl/tree/0.6.8) (2019-07-29)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.6.7...0.6.8)

**Closed issues:**

- Avoid adding clje test dirs twice to code path [\#57](https://github.com/clojerl/rebar3_clojerl/issues/57)
- test command should compile the project [\#50](https://github.com/clojerl/rebar3_clojerl/issues/50)

**Merged pull requests:**

- Prepare release 0.6.8 [\#59](https://github.com/clojerl/rebar3_clojerl/pull/59) ([jfacorro](https://github.com/jfacorro))
- \[\#57\] Avoid adding test dirs twice [\#58](https://github.com/clojerl/rebar3_clojerl/pull/58) ([jfacorro](https://github.com/jfacorro))

## [0.6.7](https://github.com/clojerl/rebar3_clojerl/tree/0.6.7) (2019-07-15)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.6.6...0.6.7)

**Closed issues:**

- Support almost empty project [\#52](https://github.com/clojerl/rebar3_clojerl/issues/52)
- Document optional entries for rebar.config [\#46](https://github.com/clojerl/rebar3_clojerl/issues/46)

**Merged pull requests:**

- \[\#50\] Process provider\_hooks for the 'compile' task [\#56](https://github.com/clojerl/rebar3_clojerl/pull/56) ([jfacorro](https://github.com/jfacorro))
- Add link to hex badge [\#55](https://github.com/clojerl/rebar3_clojerl/pull/55) ([jfacorro](https://github.com/jfacorro))
- Add hex badge [\#54](https://github.com/clojerl/rebar3_clojerl/pull/54) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#52\] Support starting a repl in an almost empty rebar.config [\#53](https://github.com/clojerl/rebar3_clojerl/pull/53) ([jfacorro](https://github.com/jfacorro))
- Include basic testing template [\#51](https://github.com/clojerl/rebar3_clojerl/pull/51) ([arpunk](https://github.com/arpunk))
- Use hex dependency in template [\#49](https://github.com/clojerl/rebar3_clojerl/pull/49) ([jfacorro](https://github.com/jfacorro))

## [0.6.6](https://github.com/clojerl/rebar3_clojerl/tree/0.6.6) (2019-07-09)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.6.5...0.6.6)

**Closed issues:**

- Document optional entries for rebar.config [\#47](https://github.com/clojerl/rebar3_clojerl/issues/47)
- Update documentation [\#42](https://github.com/clojerl/rebar3_clojerl/issues/42)
- Define sane defaults when there is no rebar.config [\#11](https://github.com/clojerl/rebar3_clojerl/issues/11)

**Merged pull requests:**

- \[Closes \#47\] document rebar3.config options [\#48](https://github.com/clojerl/rebar3_clojerl/pull/48) ([jfacorro](https://github.com/jfacorro))
- Prepare release 0.6.6 [\#45](https://github.com/clojerl/rebar3_clojerl/pull/45) ([jfacorro](https://github.com/jfacorro))
- clojerl test task is run in the test profile [\#44](https://github.com/clojerl/rebar3_clojerl/pull/44) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#42\] update documentation and template [\#43](https://github.com/clojerl/rebar3_clojerl/pull/43) ([jfacorro](https://github.com/jfacorro))
- Fix rebar3 template [\#41](https://github.com/clojerl/rebar3_clojerl/pull/41) ([arpunk](https://github.com/arpunk))

## [0.6.5](https://github.com/clojerl/rebar3_clojerl/tree/0.6.5) (2018-11-21)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.6.4...0.6.5)

**Closed issues:**

- Add template for a clojerl project  [\#38](https://github.com/clojerl/rebar3_clojerl/issues/38)

**Merged pull requests:**

- \[\#38\] Mention templates in README [\#40](https://github.com/clojerl/rebar3_clojerl/pull/40) ([jfacorro](https://github.com/jfacorro))
- \[\#38\] add template [\#39](https://github.com/clojerl/rebar3_clojerl/pull/39) ([jfacorro](https://github.com/jfacorro))

## [0.6.4](https://github.com/clojerl/rebar3_clojerl/tree/0.6.4) (2018-09-17)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.6.3...0.6.4)

**Fixed bugs:**

- Exit with failure when there are failed tests or errors [\#36](https://github.com/clojerl/rebar3_clojerl/issues/36)

**Closed issues:**

- OTP 21 compatible [\#34](https://github.com/clojerl/rebar3_clojerl/issues/34)

**Merged pull requests:**

- \[Closes \#36\] Check failures or errors and exit if any [\#37](https://github.com/clojerl/rebar3_clojerl/pull/37) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#34\] WITH\_STACKTRACE macro for OTP 21 [\#35](https://github.com/clojerl/rebar3_clojerl/pull/35) ([jfacorro](https://github.com/jfacorro))

## [0.6.3](https://github.com/clojerl/rebar3_clojerl/tree/0.6.3) (2018-08-27)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.6.2...0.6.3)

**Fixed bugs:**

- Apply compile order to deps [\#32](https://github.com/clojerl/rebar3_clojerl/issues/32)

**Merged pull requests:**

- \[Fixes \#32\] Compile order deps [\#33](https://github.com/clojerl/rebar3_clojerl/pull/33) ([jfacorro](https://github.com/jfacorro))

## [0.6.2](https://github.com/clojerl/rebar3_clojerl/tree/0.6.2) (2018-08-25)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.6.1...0.6.2)

**Fixed bugs:**

- Check for clojerl compilation  [\#30](https://github.com/clojerl/rebar3_clojerl/issues/30)

**Merged pull requests:**

- \[\#30\] Check for Clojerl compilation [\#31](https://github.com/clojerl/rebar3_clojerl/pull/31) ([jfacorro](https://github.com/jfacorro))

## [0.6.1](https://github.com/clojerl/rebar3_clojerl/tree/0.6.1) (2018-08-20)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.6.0...0.6.1)

**Merged pull requests:**

- Assume a single target from the source file [\#29](https://github.com/clojerl/rebar3_clojerl/pull/29) ([jfacorro](https://github.com/jfacorro))
- Fix dialyzer warnings [\#27](https://github.com/clojerl/rebar3_clojerl/pull/27) ([jfacorro](https://github.com/jfacorro))

## [0.6.0](https://github.com/clojerl/rebar3_clojerl/tree/0.6.0) (2018-08-08)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.5.0...0.6.0)

**Merged pull requests:**

- Adapt plugin to be able to compile Clojerl with it [\#26](https://github.com/clojerl/rebar3_clojerl/pull/26) ([jfacorro](https://github.com/jfacorro))
- Include apps finding clojerl [\#25](https://github.com/clojerl/rebar3_clojerl/pull/25) ([jfacorro](https://github.com/jfacorro))

## [0.5.0](https://github.com/clojerl/rebar3_clojerl/tree/0.5.0) (2018-07-10)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.4.0...0.5.0)

**Closed issues:**

- Add command to run tests [\#12](https://github.com/clojerl/rebar3_clojerl/issues/12)

**Merged pull requests:**

- \[Closes \#12\] Run tests command [\#24](https://github.com/clojerl/rebar3_clojerl/pull/24) ([jfacorro](https://github.com/jfacorro))

## [0.4.0](https://github.com/clojerl/rebar3_clojerl/tree/0.4.0) (2018-05-20)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.3.0...0.4.0)

**Implemented enhancements:**

- Handle protocol compilation correctly [\#22](https://github.com/clojerl/rebar3_clojerl/issues/22)

**Fixed bugs:**

- Compile task doesn't fail on error [\#20](https://github.com/clojerl/rebar3_clojerl/issues/20)

**Merged pull requests:**

- \[Closes \#22\] Handle issues with protocol compilation [\#23](https://github.com/clojerl/rebar3_clojerl/pull/23) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#20\] Abort on compiler error [\#21](https://github.com/clojerl/rebar3_clojerl/pull/21) ([jfacorro](https://github.com/jfacorro))

## [0.3.0](https://github.com/clojerl/rebar3_clojerl/tree/0.3.0) (2018-04-23)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.2.2...0.3.0)

**Closed issues:**

- Define \*compile-protocols-path\* in order to handle latest changes [\#18](https://github.com/clojerl/rebar3_clojerl/issues/18)

**Merged pull requests:**

- \[Closes \#18\] Define \*compile-protocols-path\* as the current project's ebin directory [\#19](https://github.com/clojerl/rebar3_clojerl/pull/19) ([jfacorro](https://github.com/jfacorro))

## [0.2.2](https://github.com/clojerl/rebar3_clojerl/tree/0.2.2) (2018-03-04)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.2.1...0.2.2)

**Implemented enhancements:**

- Print all of stacktrace on error only when debugging  [\#15](https://github.com/clojerl/rebar3_clojerl/issues/15)

**Merged pull requests:**

- Use clj\_utils:stacktrace/1 [\#17](https://github.com/clojerl/rebar3_clojerl/pull/17) ([jfacorro](https://github.com/jfacorro))

## [0.2.1](https://github.com/clojerl/rebar3_clojerl/tree/0.2.1) (2018-02-08)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.2.0...0.2.1)

**Merged pull requests:**

- \[\#15\] Only print stacktrace when debugging [\#16](https://github.com/clojerl/rebar3_clojerl/pull/16) ([jfacorro](https://github.com/jfacorro))

## [0.2.0](https://github.com/clojerl/rebar3_clojerl/tree/0.2.0) (2017-12-09)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/0.1.0...0.2.0)

**Implemented enhancements:**

- Add options to the repl task [\#3](https://github.com/clojerl/rebar3_clojerl/issues/3)

**Merged pull requests:**

- \[Closes \#3\] Repl options [\#13](https://github.com/clojerl/rebar3_clojerl/pull/13) ([jfacorro](https://github.com/jfacorro))

## [0.1.0](https://github.com/clojerl/rebar3_clojerl/tree/0.1.0) (2017-10-29)

[Full Changelog](https://github.com/clojerl/rebar3_clojerl/compare/8f6ddea06be9d18b5ab36ce22d44fab8d1507e56...0.1.0)

**Implemented enhancements:**

- Show stacktrace on error [\#6](https://github.com/clojerl/rebar3_clojerl/issues/6)
- Don't re-compile a file if its source hasn't changed [\#4](https://github.com/clojerl/rebar3_clojerl/issues/4)

**Fixed bugs:**

- Show errors when compiling [\#2](https://github.com/clojerl/rebar3_clojerl/issues/2)

**Closed issues:**

- Compile deps too [\#8](https://github.com/clojerl/rebar3_clojerl/issues/8)

**Merged pull requests:**

- \[Closes \#4\] Don't recompile unmodified source files \[Closes \#8\] Compile deps [\#9](https://github.com/clojerl/rebar3_clojerl/pull/9) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#6\] Include stacktrace in errors [\#7](https://github.com/clojerl/rebar3_clojerl/pull/7) ([jfacorro](https://github.com/jfacorro))
- \[Closes \#2\] Print compile-time errors [\#5](https://github.com/clojerl/rebar3_clojerl/pull/5) ([jfacorro](https://github.com/jfacorro))
- Add repl command [\#1](https://github.com/clojerl/rebar3_clojerl/pull/1) ([jfacorro](https://github.com/jfacorro))



\* *This Changelog was automatically generated by [github_changelog_generator](https://github.com/github-changelog-generator/github-changelog-generator)*
