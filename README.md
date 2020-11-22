# _c4f_: the C4 fuzzer

![Main workflow](https://github.com/MattWindsor91/act/workflows/Main%20workflow/badge.svg)

_c4f_ (previously known as `act`, bear with us as we slowly rename!)
is a 'fuzzer' over C litmus tests of a format broadly compatible with
that produced by [memalloy](https://github.com/JohnWickerson/memalloy)
and consumed by [herdtools7](https://github.com/herd/herdtools7).
It takes initial litmus tests, and performs transformations to them
that complicate their compilation while (hopefully!) preserving the
soundness of the initial test's postcondition.

Other tools in the C4 project include:

- [c4t](https://github.com/MattWindsor91/act-tester) (or 'act-tester'),
  for running automated compiler tests using _c4f_;
- [c4-scripts](https://github.com/MattWindsor91/act-bash)
  (or 'act-bash'),
  for doing various tasks that _c4f_ and _c4t_ are too low-level to do
  comfortably.

## Licence and Acknowledgements

- The overall ACT project, and all original code, is licenced under
  the MIT licence: see `LICENSE`.

- The C lexer and parser are based on those from
  [herdtools7](https://github.com/herd/herdtools7).  We include these in
  _c4f_ under the provisos of herd's
  [CECILL-B](http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html)
  licence: see `LICENSE.herd`. _(This is not an
  official endorsement by the Herd team of this project.)_

## Included tools

These will be renamed from `act-X` to `c4f-X` at some unspecified time in the future.

- `act-fuzz`, the fuzzer itself, with commands to run the fuzzer, replay fuzzer traces,
  and bisect fuzzer traces;
- `act-c`, a toolset for interpreting C litmus tests and dumping them to executable C.

Each command has various subcommands, including the `help` command: for example,
`act-fuzz help` will tell you information about which subcommands the fuzzer has.

## Building and running

**NOTE:**
_c4f_ uses Jane Street's `core` library, which only works properly on
POSIX-style operating systems.  If you're using Windows, consider
using WSL, or Cygwin, etc.

There are two main ways to build and run _c4f_:

- run `opam install .` (or `make install`) to install it on your machine as a pinned OPAM package;
  then, if your OPAM switch is in PATH, just run the tools directly;
- run its commands as `dune exec TOOLNAME -- ...`; this will automatically build _c4f_
  for you when needed, but won't work well with _c4f_'s scripts, and won't
  automatically install dependencies. (The `--` is needed to stop `dune` from
  trying to parse the arguments itself.)

You can also manually build _c4f_ using `dune build`, or
`make` (which just calls the former).

- To run test subjects as simulations using Herd, or on machines using Litmus7,
  you'll need [herdtools7](https://github.com/herd/herdtools7) (which is
  [available on OPAM](https://opam.ocaml.org/packages/herdtools7/)).
- For running compiler tests, you'll need at least one C11 compiler.


### Preparation

_c4f_ currently expects a configuration file listing fuzzer parameters
(by default, it looks in `./act.conf`).  If you're using _c4t_, it sets
up the configuration itself; if not, the following is a good first start:

```
fuzz {
  # replace N with the number of cores on the target machine
  set param cap.threads to N
}
```

### Basic usage

To test whether _c4f_ is installed and working, you can try:

`$ act-fuzz run $PATH_TO_C4F/examples/c_litmus/memalloy/test_6.litmus`

(or `dune exec act-fuzz -- run`, per above)

This should output some truly horrendous C to stdout.  (You can
experiment with the other `test_X.litmus` files in that directory,
supply your own Litmus test, or add `-o path.litmus` to write to a
file.)
