.. _conf-file-default:

The ``default`` block
---------------------

The top-level ``default`` block lets you specify defaults for various ACT
components.

Example
^^^^^^^

.. code-block:: none

   default {
     try arch x86.att
     try machine local
     try compiler local.gcc.x86.normal
     try simulator herd
   }

The ``try`` directive
^^^^^^^^^^^^^^^^^^^^^

Each element in a ``default`` block has the format ``try CATEGORY ID``.
When ACT needs to find a default element for a given ``CATEGORY``, it will
*try* each suggestion given in the ``default`` block, if one exists, from top
to bottom.

.. note::

   This means that ``try`` directives towards the top of the ``default`` block
   have precedence over ``try`` directives further down.

The possible categories are as follows:

``arch``
    Specifies the architecture to use in assembly commands if no
    ``-arch`` or ``-compiler`` command-line argument is given.

``compiler``
    Specifies the compiler to use in assembly commands if no ``-arch`` or
    ``-compiler`` command-line argument is given.

    .. warning::

       This has precedence over any default ``arch``.

``machine``
    Specifies the machine to use for compilers that don't otherwise
    specify a machine.

``simulator``
    Specifies the simulator to use when no other simulator is given.
