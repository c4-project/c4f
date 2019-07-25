(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-model: expressions *)

open Base

type t [@@deriving sexp, compare, equal]
(** Opaque type of expressions. *)

(** {2 Binary operators} *)

module Bop : sig
  (** Enumeration of binary operators. *)
  type t = Eq | L_and | L_or [@@deriving sexp, compare, equal]

  val eq : t
  (** [eq] is the equality operator. *)

  val l_and : t
  (** [l_and] is the logical AND operator. *)

  val l_or : t
  (** [l_or] is the logical OR operator. *)
end

(** {2 Constructors} *)

val atomic_load : Atomic_load.t -> t
(** [atomic_load a] lifts an atomic load [a] to an expression. *)

val bool_lit : bool -> t
(** [bool_lit b] lifts a Boolean literal [b] to an expression. *)

val int_lit : int -> t
(** [int_lit i] lifts an integer literal [i] to an expression. *)

val constant : Constant.t -> t
(** [constant k] lifts a constant [k] to an expression. *)

val bop : Bop.t -> t -> t -> t
(** [bop b l r] builds an arbitrary bop expression. *)

val eq : t -> t -> t
(** [eq l r] builds an equality expression. *)

val l_and : t -> t -> t
(** [l_and l r] builds a logical AND expression. *)

val l_or : t -> t -> t
(** [l_or l r] builds a logical OR expression. *)

val lvalue : Lvalue.t -> t
(** [lvalue lv] lifts a lvalue [lv] to an expression. *)

(** {2 Accessors} *)

val reduce :
     t
  -> constant:(Constant.t -> 'a)
  -> lvalue:(Lvalue.t -> 'a)
  -> atomic_load:(Atomic_load.t -> 'a)
  -> bop:(Bop.t -> 'a -> 'a -> 'a)
  -> 'a
(** [reduce expr ~constant ~lvalue ~atomic_load ~bop] recursively reduces
    [expr] to a single value, using the given functions at each
    corresponding stage of the expression tree. *)

(** {2 Traversals} *)

(** Traversing over atomic-action addresses in expressions. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

(** Traversing over identifiers in expressions. *)
module On_identifiers :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Act_common.C_id.t

(** Traversing over lvalues in expressions. *)
module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t

(** {2 Generation and quickchecking}

    See {{!Expression_gen} Expression_gen}. *)

val quickcheck_observer : t Base_quickcheck.Observer.t
(** Blanket observer for all expression generators. *)

(** {2 Type checking} *)

include
  Types.S_type_checkable with type t := t
(** Type-checking for expressions. *)

(** {2 Evaluating expressions} *)

module Eval : sig
  val as_constant :
    t -> env:(Address.t -> Constant.t Or_error.t) -> Constant.t Or_error.t
  (** [as_constant expr ~env] evaluates expression [expr] in the context of
      the abstract environment (store/heap) model [env], returning a
      constant if the evaluation succeeded or an error otherwise. *)

  val empty_env : Address.t -> Constant.t Or_error.t
  (** [empty_env] maps all lvalues to errors. *)
end
