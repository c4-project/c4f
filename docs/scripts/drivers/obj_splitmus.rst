.. _scripts-test-drivers-obj_splitmus:

``obj_splitmus``: spliced Litmus test driver
============================================

``obj_splitmus`` is a test driver that wraps Litmus7-style backends to work
around the restrictions in those tools' input.  It:

- de-litmusifies and compiles a C litmus test directly into an object file;
- runs Litmus7 on a stubbed-out approximation of a litmus test to generate 
  a set of C files that, once altered by splicing in the correct assembly
  directives, produces a harness for running the litmus test on physical
  hardware;
- performs the required splicing;
- runs the resulting harness.

.. warning::

   ``obj_splitmus`` is experimental (even for ACT standards) and subject to
   breaking change.

Usage
-----

For single-file usage:

.. code-block:: console

   $ ./scripts/obj_splitmus LITMUS_ID COMPILER_ID FILE

where ``LITMUS_ID`` is the fully-qualified ID of a Litmus-style backend in
``act.conf``; ``COMPILER_ID`` is the fully-qualified ID of a compiler in
``act.conf``, and ``file`` is a C litmus test.

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

``obj_splitmus`` implements the latter idea.
