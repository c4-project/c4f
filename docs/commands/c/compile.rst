.. _commands-act-c-compile:

``act-c compile``: run a compiler
---------------------------------

The ``act-c compile`` command runs a compiler, given its fully qualified identifier,
on a single input file.  It either outputs an assembly (``.s``) file (the
default), or an object (``.o``) file.

.. note::

   This command doesn't support passing additional arguments to the
   compiler, or overriding the arguments that ACT automatically passes.

Usage
^^^^^

.. code-block:: console

   $ act-c compile -compiler COMPILER_ID [-output OUTFILE] [-mode MODE] INFILE

This command also takes the usual standard arguments.
