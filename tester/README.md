# Tester

This module provides the main `act` compiler tester, as used in the
`act test` subcommand.

The modules described in here run one or more compilers, across one or
more machines, on a set of Memalloy-style C test cases with
corresponding litmus files.  They then convert the resulting assembly
into a litmus test, and (optionally) run Herd on the two tests,
comparing the state sets emitted.

These modules take the actual components used to do the testing---
compilers, assembly job runners, and various other pieces of
configuration---as parameters.  In `act`, most of these are filled in
at the top-level, ie `Bin`.