.. _commands-act-fuzz:

``act-fuzz``: the ACT fuzzer
============================

The ACT fuzzer, ``act-fuzz``, adds nonsense to C litmus tests, producing larger (but
hopefully observationally-refining) C litmus tests that exercise more
of a C compiler's control flows.

- To run the fuzzer on a single C litmus test, use the
  `run <commands-act-fuzz-run>` subcommand.
- To replay a trace from a previous fuzzer run, use the
  `replay <commands-act-fuzz-replay>` subcommand.
- To check the list of available fuzzer actions and their currently configured weights,
  use the `list-actions <commands-act-fuzz-list-actions>` command.

.. toctree::
   :maxdepth: 2
   :caption: Sub-commands:

   replay
   run
   list-actions
