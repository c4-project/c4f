(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: expressions *)

open Base
open Import

(** Opaque type of expressions. *)
type t [@@deriving sexp, compare, equal]

(** {1 Accessors}

    These will eventually replace constructors. *)

module Acc : sig
  val address : ('i, Address.t, t, [< variant]) Accessor.Simple.t
  (** [address] focuses on address expressions. *)

  val atomic : ('i, t Atomic_expression.t, t, [< variant]) Accessor.Simple.t
  (** [atomic] focuses on atomic expressions. *)

  val constant : ('i, Constant.t, t, [< variant]) Accessor.Simple.t
  (** [constant] focuses on constant expressions. *)

  val bop : ('i, Op.Binary.t * t * t, t, [< variant]) Accessor.Simple.t
  (** [bop] focuses on binary operation expressions. *)

  val uop : ('i, Op.Unary.t * t, t, [< variant]) Accessor.Simple.t
  (** [uop] focuses on unary operation expressions. *)

  val ternary : ('i, t Expr_ternary.t, t, [< variant]) Accessor.Simple.t
  (** [ternary] focuses on ternary expressions. *)
end

(** {1 Constructors} *)

val address : Address.t -> t
(** [address ad] lifts an address [ad] to an expression. *)

val atomic : t Atomic_expression.t -> t
(** [atomic a] lifts an atomic expression [a] to an expression. *)

val constant : Constant.t -> t
(** [constant k] lifts a constant [k] to an expression. *)

val bop : Op.Binary.t -> t -> t -> t
(** [bop operator l_operand r_operand] builds an arbitrary binary operator
    expression. *)

val uop : Op.Unary.t -> t -> t
(** [uop operator operand] builds an arbitrary unary operator expression. *)

val ternary : t Expr_ternary.t -> t
(** [ternary t] lifts a ternary expression [t] to an expression. *)

(** {2 Convenience constructors} *)

val atomic_cmpxchg : t Atomic_cmpxchg.t -> t
(** [atomic_cmpxchg a] lifts an atomic compare-exchange [a] to an expression. *)

val atomic_fetch : t Atomic_fetch.t -> t
(** [atomic_fetch a] lifts an atomic fetch [a] to an expression. *)

val atomic_load : Atomic_load.t -> t
(** [atomic_load a] lifts an atomic load [a] to an expression. *)

val eq : t -> t -> t
(** [eq l r] builds an equality expression. *)

val ne : t -> t -> t
(** [ne l r] builds a non-equality expression. *)

val lt : t -> t -> t
(** [lt l r] builds a less-than expression. *)

val le : t -> t -> t
(** [le l r] builds a less-than-or-equal expression. *)

val ge : t -> t -> t
(** [ge l r] builds a greater-than-or-equal expression. *)

val gt : t -> t -> t
(** [gt l r] builds a greater-than expression. *)

(** {2 Logical connectives} *)

val l_and : t -> t -> t
(** [l_and l r] builds a logical AND expression. It does not simplify. *)

val l_or : t -> t -> t
(** [l_or l r] builds a logical OR expression. It does not simplify. *)

val l_not : t -> t
(** [l_not x] builds a logical NOT over [x]. It does not simplify. *)

(** {2 Arithmetic operations} *)

val add : t -> t -> t
(** [add l r] builds an addition. It does not simplify. *)

val sub : t -> t -> t
(** [sub l r] builds a subtraction. It does not simplify. *)

(** {2 lvalues, and variables}

    Also see [address] in {!Expression_types.S}. *)

val lvalue : Lvalue.t -> t
(** [lvalue lv] lifts a lvalue [lv] to an expression. *)

val variable : C4f_common.C_id.t -> t
(** [variable v] lifts a variable reference [v] to an expression. *)

val of_variable_str_exn : string -> t
(** [of_variable_str_exn vs] tries to construct a reference to a variable
    whose name is the string [vs]. It fails with an exception if [vs] is an
    invalid C identifier. This function is for use mainly in testing, and
    shouldn't be used on user-supplied C code. *)

(** {2 Literals}

    Also see [constant] in {!Expression_types.S}. *)

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

  val ( != ) : t -> t -> t
  (** [x != y] is [ne x y]. *)

  val ( < ) : t -> t -> t
  (** [x < y] is [lt x y]. *)

  val ( <= ) : t -> t -> t
  (** [x <= y] is [le x y]. *)

  val ( >= ) : t -> t -> t
  (** [x >= y] is [ge x y]. *)

  val ( > ) : t -> t -> t
  (** [x > y] is [gt x y]. *)

  val ( + ) : t -> t -> t
  (** [x + y] is [add x y]. *)

  val ( - ) : t -> t -> t
  (** [x - y] is [sub x y]. *)

  val ( && ) : t -> t -> t
  (** [x && y] is [l_and x y]. *)

  val ( || ) : t -> t -> t
  (** [x || y] is [l_or x y]. *)

  val ( ! ) : t -> t
  (** [! x] is [l_not x]. *)
end

(** {1 Accessors} *)

val reduce_step :
     t
  -> constant:(Constant.t -> 'a)
  -> address:(Address.t -> 'a)
  -> atomic:(t Atomic_expression.t -> 'a)
  -> bop:(Op.Binary.t -> t -> t -> 'a)
  -> uop:(Op.Unary.t -> t -> 'a)
  -> ternary:(t Expr_ternary.t -> 'a)
  -> 'a
(** [reduce_step expr ~constant ~lvalue ~atomic ~bop ~uop ~ternary] reduces
    [expr] to a single value, using the given functions at each corresponding
    stage of the expression tree. *)

val reduce :
     t
  -> constant:(Constant.t -> 'a)
  -> address:(Address.t -> 'a)
  -> atomic:('a Atomic_expression.t -> 'a)
  -> bop:(Op.Binary.t -> 'a -> 'a -> 'a)
  -> uop:(Op.Unary.t -> 'a -> 'a)
  -> ternary:('a Expr_ternary.t -> 'a)
  -> 'a
(** [reduce expr ~constant ~address ~atomic ~bop ~uop ~ternary] recursively
    reduces [expr] to a single value, using the given functions at each
    corresponding stage of the expression tree. *)

(** Primitive map for applicative traversal of expressions. *)
module Base_map (Ap : Applicative.S) : sig
  val bmap :
       t
    -> constant:(Constant.t -> Constant.t Ap.t)
    -> address:(Address.t -> Address.t Ap.t)
    -> atomic:(t Atomic_expression.t -> t Atomic_expression.t Ap.t)
    -> bop:(Op.Binary.t * t * t -> (Op.Binary.t * t * t) Ap.t)
    -> uop:(Op.Unary.t * t -> (Op.Unary.t * t) Ap.t)
    -> ternary:(t Expr_ternary.t -> t Expr_ternary.t Ap.t)
    -> t Ap.t
end

(** {1 Generation and quickchecking}

    See {!Expression_gen}. *)

val quickcheck_observer : t Base_quickcheck.Observer.t
(** Blanket observer for all expression generators. *)

(** {1 Type checking} *)

(** Type-checking for expressions. *)
include Types.S_type_checkable with type t := t
