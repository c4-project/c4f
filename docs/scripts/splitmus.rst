``splitmus``: spliced Litmus
============================

``splitmus`` is a script that wraps Litmus7-style simulators to work around
the restrictions in those tools' input.  It:

- runs Litmus7 on a stubbed-out approximation of a litmus test to generate 
  a set of C files that, once altered by splicing in the correct assembly
  directives, produces a harness for running the litmus test on physical
  hardware;
- runs :doc:`../commands/asm/gen_stubs` to generate said assembly directives;
- performs the required splicing;
- runs the resulting harness.

The idea is that, eventually, ``splitmus`` will be identical in invocation to
a 'normal' run of Litmus7.

.. warning::

   ``splitmus`` is experimental (even for ACT standards) and subject to
   breaking change.

.. note::

   Limitations in ACT mean that ``splitmus`` requires C litmus tests as input.
   Eventually, once ACT is able to accept assembly litmus tests in the commands
   that ``splitmus`` needs to run, this will change to accepting in assembly
   litmus files directly.

Usage
-----

.. code-block:: console

   $ ./scripts/splitmus COMPILER FILE

where ``COMPILER`` is the fully-qualified ID of a compiler in ``act.conf``,
and ``file`` is a C litmus test.

Rationale
---------

Using Litmus7 directly (the intended way) means that our assembly litmus
tests must pass through Litmus7's assembly parser.  As Litmus7 was designed for
exploring carefully crafted litmus tests, its parser isn't designed to handle
many of the assembly constructs that compilers output.

To handle this problem, we can either:

- sanitise the assembly beforehand;
- use Litmus7 in a way that lets us bypass sending the actual assembly through
  the parser, but re-uses Litmus7's execution harness generation as much as
  possible.

``splitmus`` implements the latter idea.
