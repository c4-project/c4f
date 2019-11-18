.. _scripts-do-memalloy-test:

``do_memalloy_test``: runs a test based on fuzzed Memalloy outputs
==================================================================

``do_memalloy_test`` is a fire-and-forget wrapper around the following
operations:

* running Memalloy through `do_memalloy_dnf <scripts-do-memalloy-dnf>`;
* fuzzing its output, and building a test, through
  `fuzz_and_test <scripts-fuzz-and-test>`.

.. warning::

   Under construction.

Usage
-----

.. code-block:: console

   $ ./scripts/do_memalloy_test
       [-c COMPILER_PREDICATE] [-m MACHINE_PREDICATE]"
       [-e NUM_MEMALLOY_EVENTS] [-k NUM_FUZZ_PASSES]"
       [-fsuwtnSUWTNqvxh?] MEMALLOY_DIR

