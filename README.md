# act: automagic compiler tormentor

ACT is a toolbox for finding
concurrency memory model discrepancies between C code and its
compiled assembly.  It can use
[memalloy](https://github.com/JohnWickerson/memalloy) as a test-case
generator, and generates litmus tests that can be
used with [herd7](https://github.com/herd/herdtools7).

[![Build Status](https://travis-ci.com/MattWindsor91/act.svg?branch=master)](https://travis-ci.com/MattWindsor91/act)
[![Documentation Status](https://readthedocs.org/projects/automagic-compiler-tormentor/badge/?version=latest)](https://automagic-compiler-tormentor.readthedocs.io/en/latest/?badge=latest)

## Licence and Acknowledgements

- The overall ACT project, and all original code, is licenced under
  the MIT licence: see `LICENSE`.

- The architecture lexers and parsers are based on those from
  [herd7](https://github.com/herd/herdtools7).  We include these in
  ACT under the provisos of herd's
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

Future versions of ACT will support more
compilers/architectures/dialects/instructions/syntax.


## Building and running

There are two main ways to build and run ACT:

- run `opam install .` to install ACT on your machine as a pinned OPAM package;
  then, if your OPAM switch is in PATH, just run `act`;
- run ACT commands as `dune exec act -- ...`; this will automatically build ACT
  for you when needed, but won't work well with ACT's scripts, and won't
  automatically install dependencies. (The `--` is needed to stop `dune` from
  trying to parse the arguments itself.)

You can also manually build ACT using `dune build`, or
`make` (which just calls the former).


### Supported operating systems

ACT uses Jane Street's `core` library, which only works properly on
POSIX-style operating systems.  If you're using Windows, consider
using WSL, or Cygwin, etc.

We've tested ACT on:

- macOS (x86-64)
- Debian buster (x86-64): note: this is tested fairly infrequently, so
  there may be regressions compared to macOS


### Requirements

At a bare minimum, you'll need:

- OCaml (4.07+; last tested with version 4.07.1);
- opam (2; last tested with version 2.0.4);
- dune (1.10+; last tested with version 1.10).

You'll need some OCaml libraries, too: ACT's `act.opam` file lists these.
If you build and install ACT using `opam install .`, OPAM will get them
automatically.  Otherwise, type `opam install DEP DEP ...` where `DEP` is a
missing dependency.


#### Additional dependencies

- To run most of ACT's scripts, you'll need GNU bash, GNU awk, and Python
  3.7 or later.
  By default, the scripts expect these to be installed in `PATH` as `bash`,
  `gawk`, and `python3`.
- To run test subjects as simulations using Herd, or on machines using Litmus7,
  you'll need [herdtools7](https://github.com/herd/herdtools7) (which is
  [available on OPAM](https://opam.ocaml.org/packages/herdtools7/)).
- For running compiler tests, you'll need at least one C11 compiler.


### Preparation

ACT expects a configuration file listing available compilers and drivers
(by default, it looks in `./act.conf`).  To create one, either:

- use the `scripts/make_conf` script; or
- copy `bin/act.conf.example` and adjust to your needs.


## How to use ACT

To see basic information on which primitive ACT commands are available, or on
how to use a specific command:

```
$ act help             # general help
$ act help SUBCOMMAND  # to get brief information about a subcommand
```

Most high-level ACT workflows take the form of scripts in the `scripts/`
directory; most come with a short comment at the top of the file.

Detailed (work-in-progress) documentation is available on
[ACT's RTD page](https://automagic-compiler-tormentor.readthedocs.io/en/latest/).
