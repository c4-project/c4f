.. _commands-act-c-replace-header:

``act-c replace-header``: replace the test header on a C litmus test
--------------------------------------------------------------------

The ``act-c replace-header`` command reads in a C litmus test and
a 'test header' JSON file (such as those created by the
`dump-header <commands-act-c-replace-header>` command), and splices the
header's information into the test.  If the new test validates, it then outputs
the test.

Usage
^^^^^

.. code-block:: console

   $ act-c replace-header -header HEADERFILE TESTFILE

This command also takes the usual standard arguments.

