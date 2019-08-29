Filtering predicates
--------------------

Certain ACT subcommands that range over multiple machines, compilers, or
backends take optional arguments that permit filtering over those
items with a predicate.  These arguments are:

- ``-m`` (scripts) or ``-filter-machines`` (core);
- ``-c`` (scripts) or ``-filter-compilers`` (core).

These arguments accept *filtering predicates*: S-expressions that tell ACT how
to choose which machines and compilers to use.  This section describes the
structure of these predicates.

.. warning::
   As with most ACT things, these languages are under construction and subject to a lot of change.

Syntax
^^^^^^

Internally, the predicates parse as `blang`_ expressions.  In practice, this
means that they can take any of the following forms:

* A primitive predicate (which depends on what we're filtering; see below);
* ``true``;
* ``false``;
* ``(and <exprs>)``, where ``<exprs>`` is a list of predicates;
* ``(or <exprs>)``, where ``<exprs>`` is a list of predicates;
* ``(not <expr>)``, where ``<expr>`` is a predicate;
* ``(if <cond> <true> <false>)``, where ``<cond>``, ``<true>``, and ``<false>`` are
  all predicates.

Primitive predicates
^^^^^^^^^^^^^^^^^^^^

For up-to-date documentation on primitive predicates, run:

.. code-block:: console

   $ act configure list-predicates

The documentation below may lag slightly behind reality.

Predicates over IDs
"""""""""""""""""""

These predicates can be used in both machine and compiler filters, using the
``id <primitive>`` form.

All ID predicates are case-insensitive and don't distinguish between tag
separators (``(is foo.bar.baz)`` and ``(is FOO/BAR/BAZ)`` match the same IDs).
Any predicate matching part of an ID matches on tag boundaries
(``(has_tag foo)`` doesn't match ``bad.food``, but matches ``bad.foo.d``).

``(is <string>)``
  true if the ID is equal to the ID represented by ``<string>``;
``(has_prefix <string>)``
  true if the ID contains the ID represented by ``<string>`` as a prefix;
``(has_tag <string>)``
  true if the ID contains the *single tag* represented by ``<string>``.

Predicates over machines
""""""""""""""""""""""""

- ``(id <predicate>)``: true if the ID predicate (see above) is true for the machine's ID;
- ``is_local``: true if the machine is local (e.g. not connected via SSH);
- ``is_remote``: true if the machine is remote (e.g. connected via SSH).

Note that ``is_local`` differs from ``(not is_remote)``, and ``is_remote`` from
``(not is_local)``, if it isn't possible to determine at filter time whether the
machine is local or remote.  In these cases, the ``is_XYZ`` predicates return
``false`` (and so their ``not`` counterparts return ``true``).

.. code-block:: lisp
   :caption: Examples:

   (id (contains "linux"))        ; select all machines with the tag 'linux' somewhere in their IDs

   is_local                       ; enables only machines that are considered 'local'

   (not is_remote)                ; enables only machines that are not considered 'remote'
                                  ; (see above for the distinction between this and is_local)

Predicates over compilers
"""""""""""""""""""""""""

``(id <predicate>)``
   true if the ID predicate (see above) is true for the compiler's ID;
``(machine <predicate>)``
   true if the machine predicate (see above) is true for the compiler's machine.

.. code-block:: lisp
   :caption: Examples:

   (id (contains "O3"))           ; select all compilers with the tag 'O3' somewhere in their IDs

   (or (id (contains "O1"))       ; select all compilers with either the tag 'O1'...
       (id (contains "O2"))       ; ...or O2...
       (id (contains "O3")))      ; ...or O3 in their IDs

   (id (has-prefix "local.gcc"))  ; select all compilers whose IDs begin with the tags 'local', 'gcc'

   (machine is_local))            ; similar to 'is_local' above, but can be combined with
                                  ; other compiler-specific predicates

Predicates over sanitiser passes
""""""""""""""""""""""""""""""""

ACT exposes a similar language for choosing which sanitiser passes to run in
commands that do sanitisation.
This language's primitives include individual passes (whose names start with a
letter) as well as sets of predicates (whose names start with a ``%``).

Since the sanitiser is constantly expanding, a list of individual passes would
quickly go out of date---for now, use ``list-predicates``.

``%default``
   expands to whichever set of passes would be used if a predicate wasn't being applied.

   .. note::
      This is _not_ implicitly included in a predicate---to add to the defaults, use
      ``(or %default ...)``, and to remove from them, use ``(and %default (not ...))``
``%standard``
   expands to the fairly-comprehensive set of passes used in most situations.
``%explain``
   expands to a small set of lightweight passes useful for cleaning up assembly while using
   the explainer.

.. _blang: https://ocaml.janestreet.com/ocaml-core/latest/doc/core_kernel/Core_kernel/Blang/
