# act: automagic compiler tormentor

`act` is a work-in-progress automatic compiler tester for finding
concurrency memory model discrepancies between C code and its
compiled assembly.  It uses
[memalloy](https://github.com/JohnWickerson/memalloy) as a test-case
generator, and will eventually generate litmus tests that can be
used with herd7.

Parts of `act` derive from the
[herdtools7](https://github.com/herd/herdtools7) project of Alglave,
Maranget, et al.

## What it does

At time of writing, it literally just runs gcc (x86 32-bit) on every C
test case in a memalloy run, then emits a slightly sanitised version of
the (AT&T syntax) assembly that herd7 can _almost_ parse.
Eventually, it'll do something useful.

## Requirements

- A recent OCaml compiler (tested with 4.07)
- opam (tested with version 2)
- dune
- Several packages, which will be pointed out by `dune` if missing
  from your `opam` environment.

(Note: For `menhirLib`, install `menhir`).

**NOTE**:
`act` uses Jane Street's `core` library, which only works properly on
POSIX-style operating systems.  If you're using Windows, consider
using WSL, or Cygwin, etc.

## Running

First, copy `compiler.spec.example` somewhere (by default, `act` looks
for it in `./compiler.spec`), and adjust to your needs.

The easiest way to run `act` is through `dune exec bin/main.exe --
ARGS`; this will build `act` if needed.  Use `dune exec bin/main.exe
-- help` for general usage.  (The `--` is needed to stop `dune` from
trying to parse the arguments itself.)

### Converting a single assembly file to a litmus test

`dune exec bin/main.exe litmusify COMPILER-NAME path/to/asm.s`

This asks `act` to try to convert the given assembly file into a
Litmus test, using the spec for compiler `COMPILER-NAME` to tell it
things about the flavour of assembly incoming.  By default, the litmus
test is printed on stdout: use `-o FILE` to override.

**NOTE**:
Since `act` won't have any information besides that inside the
assembly file, the litmus output won't have any postconditions or
initial assignments, and `act` will make incomplete guesses about
where program boundaries and interesting memory locations are.

### Analysing an assembly file without conversion

`dune exec bin/main.exe explain COMPILER-NAME path/to/asm.s`

This asks `act` to dump out the given assembly file along with
line-by-line annotations explaining how `act` categorised each line of
assembly.  This is mostly useful for debugging what `act` is doing.

### Processing a memalloy run

`dune exec bin/main.exe memalloy path/to/memalloy/results`

This asks `act` to run the compilers listed in `./compiler.spec` on
the witnesses in `path/to/memalloy/run/C`, then convert them into
litmus tests in the same way that `litmusify` would.

By default, `act` will dump its results in directories in the current
working directory.

### Other possibilities

- `dune runtest` will run `act`'s _expects_ tests, and output any
  discrepancies in `act`'s output as diffs.

## Licence and Acknowledgements

- The overall `act` project, and all original code, is licenced under
  the MIT licence: see `LICENSE`.

- The architecture lexers and parsers are based on those from
  [herd7](https://github.com/herd/herdtools7).  We include these in
  `act` under the provisos of herd's
  [CECILL-B](http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html)
  licence.
