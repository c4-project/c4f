# Unreleased

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
