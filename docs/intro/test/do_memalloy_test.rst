.. _intro-test-do-memalloy-test:

Running a Memalloy-based test cycle
-----------------------------------

The most likely way in which you'll want to use ACT is by constructing a batch
of small C litmus tests; using `the ACT fuzzer <intro-fuzz>` to extrude them
into large C litmus tests; building a test run over those litmus tests and a
filtered subset of the local compilers, with Litmus as the backend; and
outputting any unexpected state observations to standard output.

ACT comes with a Bash script, ``do_memalloy_test``, which performs this whole
cycle automatically.
