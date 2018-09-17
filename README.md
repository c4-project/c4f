# Untitled automated compiler testing project

This repository, while not containing anything particularly useful
yet, will eventually hold a tool for automatically testing compilers
for concurrency memory model bugs, using
[memalloy](https://github.com/JohnWickerson/memalloy) as a test-case
generator.

Parts of `act` derive from the
[herdtools7](https://github.com/herd/herdtools7) project of Alglave,
Maranget, et al.

## What it does

At time of writing, it literally just runs a compiler on every C test
case in a memalloy run.  Eventually, it'll do something useful.

## Requirements

- opam
- dune
- ??

## Running

First, copy and modify `compiler.spec.example` to taste.

`dune exec bin/main.exe path/to/memalloy/run` will run the compilers
listed in `./compiler.spec` on `path/to/memalloy/run/`.

Use `dune exec bin/main.exe -help` for usage.

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