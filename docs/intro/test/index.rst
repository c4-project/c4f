.. _intro-test:

Running tests
=============

Once `ACT is set up <intro-before-use>` and a set of
`test subjects <intro-data>` has been produced, we can start running compiler
tests.

At the lowest level, the creation, execution, and inspection of compiler tests
are separate actions, and each has a `specific script <scripts-test-pipeline>`
that performs the appropriate actions.  This allows some flexibility in how to
handle tests: for example, the results from one long-running test can be
inspected for compilation failures, then inspected again for postcondition
failures at a later date, without re-running the test.
