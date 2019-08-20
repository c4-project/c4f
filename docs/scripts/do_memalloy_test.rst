.. _scripts-do-memalloy-test:

``do_memalloy_test``: runs a test based on fuzzed Memalloy outputs
==================================================================

``do_memalloy_test`` is a fire-and-forget wrapper around the following
operations:

* running Memalloy itself through `do_memalloy <scripts-do-memalloy>`;
* fuzzing the results using `fuzz_many <scripts-fuzz-many>`;
* building a test using `make_test <scripts-make-test>`;
* running the test using `run_test <scripts-run-test>`;
* ???;
* profit!

.. warning::

   Under construction.  At time of writing, only the first bit actually
   works.
