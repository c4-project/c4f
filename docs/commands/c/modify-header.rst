.. _commands-act-c-modify-header:

``act-c modify-header``: change parts of a test header on a C litmus test
-------------------------------------------------------------------------

The ``act-c modify-header`` command, like
`replace-header <commands-act-c-replace-header>`, changes the header information
in a C litmus test.  Unlike ``replace-header``, which completely substitutes a
header JSON file for the test header, ``modify-header`` accepts
specific patches to the test header on the command line.

Usage
^^^^^

.. code-block:: console

   $ act-c modify-header [patches] TESTFILE

where the possible ``patches`` are:

- ``-name NAME``: replace the name of the test with ``NAME``;
- ``-no-postcondition``: delete any existing postcondition in the test;
- ``-postcondition PROP``: replace the postcondition of the test with ``PROP``.

This command also takes the usual standard arguments.
