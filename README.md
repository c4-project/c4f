# Untitled automated compiler testing project

This repository, while not containing anything particularly useful yet, will eventually hold a tool for automatically testing compilers for concurrency memory model bugs, using [memalloy](https://github.com/JohnWickerson/memalloy) as a test-case generator.

## What it does

At time of writing, it literally just runs a compiler on every C test case in a memalloy run.  Eventually, it'll do something useful.

## Requirements

- opam
- dune
- ??

## Running

`dune exec bin/main.exe path/to/memalloy/run`

Use `dune exec bin/main.exe -help` for usage.

By default, `act` will dump its results in directories in the current working directory.
