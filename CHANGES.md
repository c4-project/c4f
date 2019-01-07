# Unreleased

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
