.. _intro-data-fuzz:

Fuzzing existing litmus tests
-----------------------------

ACT contains a tool, ``act-fuzz``, for taking C litmus tests and
randomly altering them in various final-state-preserving ways.  This is useful,
for example, for expanding the small tests output by
:ref:`Memalloy <intro-data-memalloy>`.

.. warning::

   ACT's fuzzer is in a highly experimental stage.  It doesn't do much yet,
   and what it *does* do can sometimes be semantically surprising.
