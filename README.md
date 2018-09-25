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

**NOTE**:
`act` uses Jane Street's `core` library, which only works properly on
POSIX-style operating systems.  If you're using Windows, consider
using WSL, or Cygwin, etc.

## Running

- First, copy and modify `compiler.spec.example` to taste.
- Use `dune exec bin/main.exe -help` for usage.
- `dune exec bin/main.exe path/to/memalloy/run` will run the compilers
  listed in `./compiler.spec` on `path/to/memalloy/run/`.
- `dune runtest` will run `act`'s _expects_ tests, and output any
  discrepancies in `act`'s output as diffs.

By default, `act` will dump its results in directories in the current
working directory.

## Licence and Acknowledgements

- The overall `act` project, and all original code, is licenced under
  the MIT licence: see `LICENSE`.

- The architecture lexers and parsers are based on those from
  [herd7](https://github.com/herd/herdtools7).  We include these in
  `act` under the provisos of herd's
  [CECILL-B](http://www.cecill.info/licences/Licence_CeCILL-B_V1-en.html)
  licence.
