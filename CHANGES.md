# Unreleased

Note that these changes are infrequently maintained and may not tell the
whole story.

## 2019-Aug-08

- State observation comparing is now available at `act-state compare`.  It
  does NOT perform normalisation of the two state sets --- this must be done
  externally (a tool for doing this using delitmus will appear eventually).
- State observation comparison results come as JSON by default; pass
  `-human-readable` for pretty-printed equivalent.

## 2019-Jul-29

- Amongst other things, `act c fuzz` is now `act-fuzz run` (ie a new program).
- Action weight listing is now `act-fuzz list-actions`.

## 2019-Jun-24

- `act tool sim` is now `act backend run`, in anticipation of a cross-board
  rename.
- New command `act backend parse`, which parses the output of a configured
  simulator and outputs an observation record as a JSON file.

## 2019-Jun-11

- New command `act c compile`, for compiling single files without testing or
  litmusifying them.

## 2019-Jun-06

- The structure of `act.conf`s has changed drastically.  The key points are:
    - Compilers, simulators, and other such potentially-remote tools are now
      nested inside machines, and referred to by the concatenation of the
      machine ID and element ID.
    - Simulator configuration no longer takes a `herd` or `litmus` stanza, but
      instead a `sim ID` stanza.  A `style ID` (where `ID` is `herd` or `litmus`
      inside the stanza tells ACT how to invoke and interpret the simulator.
    - `emits` is now `arch`.
    - There is a `default` stanza (for specifying which architecture,
      compiler, machine, etc. to try by default), but it isn't yet usable.
      Hooking this up to the resolution systems is future work.
- Bugs and oddities caused by the above changes are currently being ironed
  out.

## 2019-May-27

- `act asm litmusify` no longer supports piping through a simulator.
  Running a simulator on a single litmus test using its simulator ID will
  become a separate feature later on.
- File type overrides are now of the form
  `-file-type X`, where `X` is `asm`, `asm-litmus`, `c`, or `c-litmus`.
  (Part of this change predates this change report.)

## 2019-May-15

- Almost all `act` modules are now publicly exposed as `act.foo` (with
  name changed from `Foo` to `Act_foo`.)

## 2019-Apr-24

- New standard flag: `colour`, which takes `always`, `never`, or `auto`
  (the default), and colourises output accordingly.  Currently, not a lot of
  `act` supports colourisation, but expect this to change.
- New(ish) command: `act configure list-fuzzer-actions`, which describes the
  fuzzer actions `act` understands; for each, it outputs a readme and the
  current configured weight.  This should make it easier to configure fuzzer
  weights.

## 2019-Apr-03

- The `fuzz_many` script in `scripts/` now takes arguments in the
  order `fuzz_many COUNT FILE*`, with multiple `FILE`s supported.  When
  given multiple files, it mutates each `COUNT` times, giving each a
  distinct filename.  It now also symbolically links `_fuzz_latest` to
  the latest fuzzer directory, in a similar manner to Memalloy.

## 2019-Mar-12

- Split `-cvars` option into two new options: `-c-globals` and
  `-c-locals`.  The difference is that `-c-globals` are used to build
  `locations` stanzas in any emitted Litmus tests, whereas
  `-c-locals` are only used to inform the sanitiser which symbols are
  variables.

## 2019-Feb-27 (and previous)

- Feature: Ongoing work on building a C11 litmus test mutator (or
  'fuzzer').  See `act c fuzz`.  Currently in a heavily experimental
  state.
- Change: `unix_test_scripts` is now `scripts`, and is starting to
  contain things that aren't test scripts per se.

## 2019-Jan-30

- Bugfix: `act c delitmus` now properly handles removing dereferences from
  global variables (or, at least, does so _more_ properly than before).
  (#64)

## 2019-Jan-29

- Feature: started bundling a series of shell scripts for
  semi-automated testing of `act`.  These complement the automated
  tests, and are intended for checking how `act` interacts with a full
  Unix environment (including working C compilers and `act.conf`s).
  Currently there is only one script, which is a work-in-progress, but
  this will likely change.
- Feature: `act c explain` now takes an optional flag `-dump-cvars`,
  which causes it to emit the list of C variables that it found in the
  input.  This is useful for passing to `-cvars` later on.
- Bugfix: `act c delitmus` now correctly infers global variable types
  from the thread functions' parameter lists.  This means that
  global variables with non-atomic `int`s now get the right type in
  the delitmusification.  (#63)

## 2019-Jan-28

- Change: in many locations where `act` expects a C-style identifier,
  such as Litmus identifiers and test names, `act` now automatically
  does validation to make sure that the identifier is indeed a proper
  C identifier string.  This may cause intermittent breakages.

## 2019-Jan-22

- Feature: litmusify now automatically finds the set of C variables
  when run on a C/litmus file (*not* plain C) and `-cvars` isn't
  provided.  This will expand to other `act` subcommands in due
  course.
- Note: to see the cvars set chosen, pass `-verbose` to litmusify.

## 2019-Jan-21

- Bugfix: delitmusifier no longer erroneously reverses its arguments.

## Undated (2018-Dec to 2019-Jan)

- Litmusify: `-litmus` now executes litmus on the remote machine.
- Added new `act tool` command, which will eventually allow free-form running
  of the various tools act wraps.  Currently it supports herd
  (`act tool herd`), but doesn't do anything special.
- Litmusify: added `-exists` flag to feed in a Litmus postcondition.
  (This currently has to target the sanitised Litmus variables directly;
  no redirection following is done yet.)
- Split `act regress` into several subcommands, and include a `delitmus`
  regression test set.
- New `act c delitmus`, which tries to convert a C/Litmus test into
  compileable C11.  There is also an `act c explain`, but at this stage it's
  fairly pointless.
- New `act configure list-predicates`, which describes the languages accepted
  by the filtering predicate commands.
- Moved `act specs` to `act configure list-compilers`.
- Added initial support for the `litmus7` tool.  This currently only stretches
  as far as configuration stanzas and piping through the `litmusify` tool.
- Added experimental C language support.  For now, use `explain-c` to test.
- Replaced the S-expression `compiler.spec` format with a more human-friendly,
  act-specific one.  See `bin/act.conf` for an example of the syntax, which is
  similar to OpenBSD's utility config format.
- Renamed `compiler.spec` to `act.conf` -- this should help users spot the
  change in format, as well as being more indicative of what the file is.
- Changelog starts here.

This changelog started on 2018-12-19, and doesn't contain any changes made
prior to that date.
