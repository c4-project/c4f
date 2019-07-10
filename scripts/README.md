# Unix scripts

This directory contains shell, Python, and AWK scripts that assist in use, and
semi-automated testing, of `act`.

Most of these tests require some sort of pre-existing environment that we can't
necessarily assume in the main tests: a POSIX shell; a working `act.conf`; a
working C compiler; various test inputs, etc.  This is why we keep them
separate from the fully-automated unit and regression tests that `dune
runtest`/`make test` runs, which make as few assumptions about the testing
environment as possible.

When used for tests, these scripts are inherently less robust and less
thoroughly used than the automated ones.

Each script contains inline documentation on how to use it.
