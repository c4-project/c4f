(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** Signatures of operations used to construct and destruct Litmus
    AST primitives.

    These signatures are generic over whether the various primitives
    have a direct, type-level parametrisation over language constants
    (as is the case in {{!Ast_base}Ast_base}), or indirectly through
    the module system (as is the case in {{!Ast}Ast}. *)

(** Signature of operations that can be used on Litmus predicate
   elements. *)
module type S_pred_elt = sig
  (** Type of identifiers. *)
  type id

  (** Type of constants, possibly parametrised directly. *)
  type 'const elt

  (** Type of predicate elements, possibly parametrised directly by
      language constants. *)
  type 'const t

  (** [l ==? r] constructs an equality constraint. *)
  val ( ==? ) : id -> 'const elt -> 'const t
end

(** Signature of operations that can be used on Litmus predicates. *)
module type S_pred = sig
  (** Type of predicates, possibly parametrised directly by
      language constants. *)
  type 'const t

  (** Type of predicate elements, possibly parametrised directly by
      language constants. *)
  type 'const elt

  (** [l || r] constructs a disjunction. *)
  val ( || ) : 'const t -> 'const t -> 'const t

  (** [l && r] constructs a conjunction. *)
  val ( && ) : 'const t -> 'const t -> 'const t

  (** [elt x] lifts [x] to a predicate. *)
  val elt : 'const elt -> 'const t

  (** [bracket x] surrounds [x] with parentheses. *)
  val bracket : 'const t -> 'const t

  (** [debracket pred] removes any brackets in [pred]. *)
  val debracket : 'const t -> 'const t
end

(** Signature of operations that can be used on Litmus postconditions. *)
module type S_postcondition = sig
  (** Type of postconditions, possibly parametrised directly by
     language constants. *)
  type 'const t

  (** Type of predicates, possibly parametrised directly by
      language constants. *)
  type 'const pred

  (** [make ~quantifier ~predicate] constructs a postcondition. *)
  val make : quantifier:[ `Exists ] -> predicate:'const pred -> 'const t

  (** [quantifier post] gets [post]'s quantifier. *)
  val quantifier : 'const t -> [ `Exists ]

  (** [predicate post] gets [post]'s predicate. *)
  val predicate : 'const t -> 'const pred
end
