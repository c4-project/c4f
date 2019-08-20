.. _scripts-do-memalloy:

``do_memalloy``: runs Memalloy with sensible defaults
=====================================================

``do_memalloy`` is just a thin layer over a locally installed instance of
Memalloy, which fills in a few parameters that are useful for using it to
generate ACT input.

The main purpose of this script is to be called by
`scripts-do-memalloy-test`.

Usage
-----

.. code-block:: console

   $ ./scripts/do_memalloy [-n NUM_EVENTS] MEMALLOY_DIR

where ``NUM_EVENTS`` is an optional maximum number of events to ask Memalloy to
generate, and ``MEMALLOY_DIR`` is the directory in which Memalloy's
``comparator`` script can be found.

.. warning::

   This script will pollute ``MEMALLOY_DIR`` with the output of the Memalloy
   run.  This behaviour may change in future.
