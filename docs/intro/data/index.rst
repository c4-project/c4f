Getting test-run data
=====================

To use ACT to test C compilers, you need a body of input data.  This takes the
form of one or more C `litmus tests`_ in the standard *herdtools* format.
Such tests should, ideally, explore
as much of the compiler's behaviour with respect to concurrency as possible.

.. toctree::
   :maxdepth: 2

   memalloy
   fuzz

.. _litmus tests: http://diy.inria.fr/doc/litmus.html#sec3 
