(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-model: expressions *)

open Base

(** Opaque type of expressions. *)
type t [@@deriving sexp, compare, equal]

(** {1 Operators} *)

(** {2 Unary operators} *)

module Uop : sig
  (** Enumeration of unary operators. *)
  type t = L_not [@@deriving sexp, compare, equal]

  val l_not : t
  (** [l_not] is the logical negation operator. *)
end

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

(** {1 Constructors} *)

val atomic_load : Atomic_load.t -> t
(** [atomic_load a] lifts an atomic load [a] to an expression. *)

val bop : Bop.t -> t -> t -> t
(** [bop operator l_operand r_operand] builds an arbitrary binary operator
    expression. *)

val uop : Uop.t -> t -> t
(** [uop operator operand] builds an arbitrary unary operator expression. *)

val eq : t -> t -> t
(** [eq l r] builds an equality expression. *)

(** {2 Logical connectives} *)

val l_and : t -> t -> t
(** [l_and l r] builds a logical AND expression. It does not simplify. *)

val l_or : t -> t -> t
(** [l_or l r] builds a logical OR expression. It does not simplify. *)

val l_not : t -> t
(** [l_not x] builds a logical NOT over [x]. It does not simplify. *)

(** {2 Addresses, lvalues, and variables} *)

val address : Address.t -> t
(** [address ad] lifts an address [ad] to an expression. *)

val lvalue : Lvalue.t -> t
(** [lvalue lv] lifts a lvalue [lv] to an expression. *)

val variable : Act_common.C_id.t -> t
(** [variable v] lifts a variable reference [v] to an expression. *)

val of_variable_str_exn : string -> t
(** [of_variable_str_exn vs] tries to construct a reference to a variable
    whose name is the string [vs]. It fails with an exception if [vs] is an
    invalid C identifier. This function is for use mainly in testing, and
    shouldn't be used on user-supplied C code. *)

(** {2 Literals} *)

val constant : Constant.t -> t
(** [constant k] lifts a constant [k] to an expression. *)

val bool_lit : bool -> t
(** [bool_lit b] lifts a Boolean literal [b] to an expression. *)

val int_lit : int -> t
(** [int_lit i] lifts an integer literal [i] to an expression. *)

val truth : t
(** [truth] is the 'true' Boolean constant. *)

val falsehood : t
(** [falsehood] is the 'false' Boolean constant. *)

(** {2 Infix constructors} *)

module Infix : sig
  val ( == ) : t -> t -> t
  (** [x == y] is [eq x y]. *)

  val ( && ) : t -> t -> t
  (** [x && y] is [l_and x y]. *)

  val ( || ) : t -> t -> t
  (** [x || y] is [l_or x y]. *)

  val ( ! ) : t -> t
  (** [! x] is [l_not x]. *)
end

(** {1 Accessors} *)

val reduce :
     t
  -> constant:(Constant.t -> 'a)
  -> address:(Address.t -> 'a)
  -> atomic_load:(Atomic_load.t -> 'a)
  -> bop:(Bop.t -> 'a -> 'a -> 'a)
  -> uop:(Uop.t -> 'a -> 'a)
  -> 'a
(** [reduce expr ~constant ~lvalue ~atomic_load ~bop ~uop] recursively
    reduces [expr] to a single value, using the given functions at each
    corresponding stage of the expression tree. *)

(** {1 Traversals} *)

(** Traversing over atomic-action addresses in expressions. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

(** Traversing over constants in expressions. *)
module On_constants :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Constant.t

(** Traversing over lvalues in expressions. *)
module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t

(** {1 Generation and quickchecking}

    See {{!Expression_gen} Expression_gen}. *)

val quickcheck_observer : t Base_quickcheck.Observer.t
(** Blanket observer for all expression generators. *)

(** {1 Type checking} *)

(** Type-checking for expressions. *)
include Types.S_type_checkable with type t := t

(** {1 Evaluating expressions} *)

module Eval : sig
  val as_constant :
    t -> env:(Address.t -> Constant.t Or_error.t) -> Constant.t Or_error.t
  (** [as_constant expr ~env] evaluates expression [expr] in the context of
      the abstract environment (store/heap) model [env], returning a constant
      if the evaluation succeeded or an error otherwise. *)

  val empty_env : Address.t -> Constant.t Or_error.t
  (** [empty_env] maps all lvalues to errors. *)
end
