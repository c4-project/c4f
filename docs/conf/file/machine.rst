The ``machine`` block
---------------------

.. warning:: Under construction.

The top-level ``machine`` block describes a machine that is available to ACT,
as well as various components available through that machine.
components.

Example
^^^^^^^

.. code-block:: none

   machine localhost {
     via local

     compiler gcc {
       # ...
     }

     sim herd {
       # ...
     }
   }
