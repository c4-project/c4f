# Unreleased

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
