.. _intro-fuzz:

Using ACT's fuzzer
==================

While the input from Memalloy has at least one statically-known
memory-model-violating behaviour, it is also often too small and too
restricted in its use of C control flows to be likely to trigger bad
compiler behaviour.  To help fix the latter without affecting the
former, ACT has a *fuzzer*: a series of tools that accept a small C
litmus test, perform a variety of random (but behaviour-refining)
modifications to its program text, and emit one or more larger C
litmus tests.

.. warning:: Under construction.

.. toctree::
   :maxdepth: 2

   config
   single
   multiple
