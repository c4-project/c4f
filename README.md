# act: automagic compiler tormentor

[![Build Status](https://travis-ci.com/MattWindsor91/act.svg?branch=master)](https://travis-ci.com/MattWindsor91/act)
[![Documentation Status](https://readthedocs.org/projects/automagic-compiler-tormentor/badge/?version=latest)](https://automagic-compiler-tormentor.readthedocs.io/en/latest/?badge=latest)

`act` is a toolbox for finding
concurrency memory model discrepancies between C code and its
compiled assembly.  It can use
[memalloy](https://github.com/JohnWickerson/memalloy) as a test-case
generator, and generates litmus tests that can be
used with [herd7](https://github.com/herd/herdtools7).


## Licence and Acknowledgements

- The overall `act` project, and all original code, is licenced under
  the MIT licence: see `LICENSE`.

- The architecture lexers and parsers are based on those from
  [herd7](https://github.com/herd/herdtools7).  We include these in
  `act` under the provisos of herd's
  [CECILL-B](http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html)
  licence: see `LICENSE.herd`. _(Note: this does *not* constitute an
  official endorsement by the Herd team of this project.)_


## What it can do (so far)

- Run various versions of x86-32 gcc/clang against a set of memalloy
  witnesses (including experimental compile-by-SSH support);
- Sanitise the assembly output by removing or simplifying pieces of
  syntax that herd7 doesn't understand (though this process is
  inherently partial and prone to issues);
- Generate litmus tests from the sanitised assembly;
- Run herd on those litmus tests, saving the results for later
  inspection.
- Other modes allow per-file sanitisation, litmus generation,
  Herd running, etc.

Future versions of `act` will support more
compilers/architectures/dialects/instructions/syntax.


## Building and running

`act` uses the `dune` build system.  The practical upshot of this is
that, so long as you run `act` through `dune exec`, `dune` will
automatically build `act` for you when needed.

You can, of course, manually build `act` using `dune build`, or
`make` (which just calls the former).


### Supported operating systems

`act` uses Jane Street's `core` library, which only works properly on
POSIX-style operating systems.  If you're using Windows, consider
using WSL, or Cygwin, etc.

We've tested `act` on:

- macOS (x86-64)
- Debian buster (x86-64): note: this is tested fairly infrequently, so
  there may be regressions compared to macOS

### Requirements

First, you'll need:

- OCaml (4.07+)
- opam (last tested with version 2)
- dune (1.8+; last tested with version 2.0.4)

To build `act`, you can either:

- ask opam to build and install `act` using `opam install .` (or similar);
- build manually using `dune build`.  You'll then need to install several
  `opam` packages;  to find out which, run
  `dune external-lib-deps --missing bin/act.exe`.

**NOTE**: For `menhirLib`, install the `menhir` OPAM package (not the
nonexistent `menhirLib` one!).

Finally, for most `act` commands, you'll need at least one C11 compiler,
and [herd7 or litmus7](https://github.com/herd/herdtools7) (which are
available on opam).

### Preparation

First, copy `bin/act.conf.example` somewhere (by default, `act`
looks for it in `./act.conf`), and adjust to your needs.


### Running

The easiest way to run `act` is through `dune exec bin/act.exe --
ARGS`; this will build `act` if needed.  Use `dune exec bin/act.exe
-- help` for general usage.  (The `--` is needed to stop `dune` from
trying to parse the arguments itself.)

To start with, try:

```
$ dune exec act -- configure list-compilers
```

This will read in your `act.conf` file, and, if all is well,
output the available compilers.


### Installing

To install `act` into your OPAM `bin` directory, try:

```
$ dune build @install
$ dune install
```

### Testing

`dune runtest` will run `act`'s test suite, and output any
discrepancies in `act`'s output as diffs.

`act` has the following types of test:

- _expects tests_, which are inlined into the code, and test the
  immediate output of various `act` functions;
- _regression tests_, which run the explainer and litmusifier on a
  directory of sample assembly and diff the result against a last
  known good output (see `bin/tests` and `bin/dune` for the tests);
- _semi-automatic tests_, which take the form of Unix shell scripts
  that simplify running various testing workflows that don't fit
  well into the automatic flow (see `scripts`).


## How to use `act`

`act` has several different subcommands.  The main ones are `act test`
(test multiple compilers on multiple C inputs) and `act litmusify`
(perform various actions on a single C file with one compiler).

To see basic information on which commands are available, or on how
to use a specific command:

```
$ act help             # general help
$ act help SUBCOMMAND  # to get brief information about a subcommand
```

Detailed documentation for each subcommand is available on the
[wiki](https://github.com/MattWindsor91/act/wiki).
