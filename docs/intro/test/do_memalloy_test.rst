.. _intro-test-do-memalloy-test:

Running a Memalloy-based test cycle
-----------------------------------

A common test workflow when using ACT involves constructing a batch
of small C litmus tests, using `the ACT fuzzer <intro-fuzz>` to extrude them
into large C litmus tests, building a test run over those litmus tests and a
filtered subset of the local compilers (with Litmus as the backend), and
outputting any unexpected state observations to standard output.  ACT supports
this workflow with additional scripting on top of its more low-level testing
primitives.

ACT comes with a Bash script, `do_memalloy_test <scripts-do-memalloy-test>`,
which performs a single run of the above cycle automatically.  While the main
doc page for the script gives full information over the script's usage and
invocation, this page discusses a typical workflow.

.. note::
   The discussion below assumes that your directories and path are set up in
   the same way as the `docker image <intro-before-use-docker>`.  You may need
   to adjust some paths appropriately.

Using ``do_memalloy_test``
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. warning::
   TODO

.. code-block:: console

   $ ./scripts/do_memalloy_test -c "(id (has_tag x64))" -m "is_local" memalloy

Inspecting the test results
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. warning::
   TODO

Running further tests on the Memalloy corpus
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

While ``do_memalloy_test`` can be run repeatedly on the same Memalloy directory
without triggering a re-run of the Memalloy and DNF stages (pass ``-f`` to
force repopulation), we can also use the lower-level
``fuzz_and_test <scripts-fuzz-and-test>`` script to spark off further fuzzing
tests using the results of those stages.

.. code-block:: console

   $ ./scripts/fuzz_and_test -c "(id (has_tag x64))" -m "is_local" \
       memalloy/results/_latest/act/dnf/*.litmus


Doing multiple Memalloy tests in parallel
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. warning::
   TODO
