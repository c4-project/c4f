(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: primitive statements.

    A primitive statement is any statement that doesn't contain other
    statements recursively.

    As a lot of FIR analysis involves whether a statement is a control flow
    or not, it makes sense to group these statements into one block.

    Note that we treat some things that are technically expressions in C as
    statements in FIR. This is for simplicity; it rejects some well-formed C
    programs, but chimes better with how we expect the expressions to be
    used. *)

open Base

(** Opaque type of FIR primitive statements. *)
type t [@@deriving sexp, compare, equal]

(** {1 Accessors} *)

val assign : ('a, Assign.t, t, [< Accessor.variant]) Accessor.t
(** [assign] focuses on assignment statements. *)

val label :
  ('a, C4f_common.C_id.t, t, [< Accessor.variant]) Accessor.t
(** [label] focuses on label statements. *)

val goto : ('a, C4f_common.C_id.t, t, [< Accessor.variant]) Accessor.t
(** [goto] focuses on goto statements. *)

val nop : ('a, unit, t, [< Accessor.variant]) Accessor.t
(** [nop] focuses on no-op (semicolon) statements. *)

val procedure_call : ('a, Call.t, t, [< Accessor.variant]) Accessor.t
(** [procedure_call] focuses on procedure (non-value-returning function) call
    statements. *)

val atomic :
  ('a, Atomic_statement.t, t, [< Accessor.variant]) Accessor.t
(** [atomic] focuses on atomic statements. *)

(** {2 Shorthand for early-out statements}

    See {!Early_out}. *)

val early_out : ('a, Early_out.t, t, [< Accessor.variant]) Accessor.t
(** [early_out] focuses on early-out statements. *)

val break : t
(** [break] is a break statement. *)

val continue : t
(** [continue] is a continue statement. *)

val return : t
(** [return] is a return statement. *)

(** {1 Traversals} *)

val value_map :
     t
  -> assign:(Assign.t -> 'result)
  -> atomic:(Atomic_statement.t -> 'result)
  -> early_out:(Early_out.t -> 'result)
  -> label:(C4f_common.C_id.t -> 'result)
  -> goto:(C4f_common.C_id.t -> 'result)
  -> nop:(unit -> 'result)
  -> procedure_call:(Call.t -> 'result)
  -> 'result
(** [value_map x ~assign ~atomic ~early_out ~label ~goto ~nop
    ~procedure_call]
    reduces a primitive statement [x] to a particular result type by applying
    the appropriate function. *)

(** Creates a basic applicative map over [M]. *)
module Base_map (M : Applicative.S) : sig
  val bmap :
       t
    -> assign:(Assign.t -> Assign.t M.t)
    -> atomic:(Atomic_statement.t -> Atomic_statement.t M.t)
    -> early_out:(Early_out.t -> Early_out.t M.t)
    -> label:(C4f_common.C_id.t -> C4f_common.C_id.t M.t)
    -> goto:(C4f_common.C_id.t -> C4f_common.C_id.t M.t)
    -> procedure_call:(Call.t -> Call.t M.t)
    -> t M.t
  (** [bmap x ~assign ~atomic ~early_out ~label ~goto ~procedure_call] maps
      the appropriate function over [x]. *)
end

(** Traverses over the lvalues of a primitive statement. *)
module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t

(** Traverses over the addresses of a primitive statement. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

(** Traverses over the expressions of a primitive statement. *)
module On_expressions :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Expression.t

(** Traverses over the atomic actions of a primitive statement. *)
module On_atomics :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Atomic_statement.t
