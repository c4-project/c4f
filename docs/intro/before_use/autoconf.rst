Generating an initial ``act.conf`` file
=======================================

Most of ACT's functionality depends on a configuration file (usually called
``act.conf``) being present; this file describes the various machines,
compilers, simulators, and other tools available to ACT.

While one can write an ``act.conf`` by hand (see :doc:`../../conf/index`
for further details), ACT ships with a ``sh`` script that automatically
writes one based on the compilers and simulators it can find on the local
machine.  One can use this as follows:

.. code-block:: console

   # Make sure that all compilers, simulators, etc. are in $PATH first!
   $ ./scripts/make_conf > act.conf

This script emits a simple ``act.conf`` straight to stdout, printing any
errors and warnings to stderr.

