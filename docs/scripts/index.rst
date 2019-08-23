*******
Scripts
*******

ACT comes with a directory of scripts (in ``bash``, ``python``, and ``awk``,
primarily) and other pieces of plumbing to help set up testing workflows.
These live in the ``./scripts`` directory.

.. warning:: Under construction.

.. note::

   Most of ACT's scripts assume that the current directory is the
   ACT project root (in other words, that the scripts are in ``./scripts``).
   This may be fixed eventually.

.. toctree::
   :maxdepth: 2
   :caption: Contents:

   do_memalloy
   do_memalloy_test
   fuzz_many
   make_conf
   test_pipeline
   test_drivers
