.. _commands-act-c-dump-header:

``act-c dump-header``: extract a test header from a C litmus test
-----------------------------------------------------------------

The ``act-c dump-header`` command reads in a C litmus test and extracts
its 'test header': the part of the test that isn't thread code.
It outputs a JSON file containing this header.

.. note::

   The JSON emitted by this command is in the same format as the
   ``litmus_header`` key of a `delitmus <commands-act-c-delitmus>` aux file.
   The difference is that this command doesn't do any renaming of variables, so
   the contents of the header are exactly those in the test.

Usage
^^^^^

.. code-block:: console

   $ act-c dump-header INFILE

This command also takes the usual standard arguments.

Example
^^^^^^^

.. code-block:: console

   $ act-c dump-header examples/c_litmus/sbsc.c.litmus
   {
     "locations": null,
     "init": { "x": 0, "y": 0 },
     "postcondition": "exists (0:a == 0 /\\ 1:a == 0)"
   }
