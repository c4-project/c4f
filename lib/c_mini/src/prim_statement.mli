(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-C: primitive statements.

    A primitive statement is any statement that doesn't contain other
    statements recursively.

    As a lot of mini-C analysis involves whether a statement is a control
    flow or not, it makes sense to group these statements into one block.

    Note that we treat some things that are technically expressions in C as
    statements in mini-C. This is for simplicity; it rejects some well-formed
    C programs, but chimes better with how we expect the expressions to be
    used. *)

open Base

(** Opaque type of mini-C primitive statements. *)
type t [@@deriving sexp, compare, equal]

(** {1 Constructors} *)

val assign : Assign.t -> t
(** [assign a] lifts an assignment [a] to a primitive statement. *)

val label : Act_common.C_id.t -> t
(** [label l] lifts a label [l] to a primitive statement. *)

val goto : Act_common.C_id.t -> t
(** [goto l] lifts a goto to label [l] to a primitive statement. *)

val nop : t
(** [nop] is a no-op (semicolon) primitive statement. *)

val procedure_call : Call.t -> t
(** [procedure_call a] lifts a procedure (non-value-returning function) call
    [a] to a primitive statement. *)

(** {2 Atomics}

    See {!Atomic_statement}. *)

val atomic : Atomic_statement.t -> t
(** [atomic a] lifts an atomic statement [a], with metadata [meta], to a
    primitive statement. *)

val atomic_cmpxchg : Expression.t Atomic_cmpxchg.t -> t
(** [atomic_cmpxchg a] lifts an atomic compare-exchange [a] to a primitive
    statement. *)

val atomic_fetch : Expression.t Atomic_fetch.t -> t
(** [atomic_fetch a] lifts an atomic fetch [a] to a primitive statement. *)

val atomic_fence : Atomic_fence.t -> t
(** [atomic_fence a] lifts an atomic fence [a] to a primitive statement. *)

val atomic_store : Atomic_store.t -> t
(** [atomic_store a] lifts an atomic store [a] to a primitive statement. *)

(** {2 Shorthand for early-out statements}

    See {!Early_out}. *)

val early_out : Early_out.t -> t
(** [early_out e] lifts an early-out statement [e] to a primitive statement. *)

val break : t
(** [break] is a break statement. *)

val continue : t
(** [continue] is a continue statement. *)

val return : t
(** [return] is a return statement. *)

(** {1 Traversals} *)

val reduce :
     t
  -> assign:(Assign.t -> 'result)
  -> atomic:(Atomic_statement.t -> 'result)
  -> early_out:(Early_out.t -> 'result)
  -> label:(Act_common.C_id.t -> 'result)
  -> goto:(Act_common.C_id.t -> 'result)
  -> nop:(unit -> 'result)
  -> procedure_call:(Call.t -> 'result)
  -> 'result
(** [reduce x ~assign ~atomic ~early_out ~label ~goto ~nop ~procedure_call]
    reduces a primitive statement [x] to a particular result type by applying
    the appropriate function. *)

val as_atomic : t -> Atomic_statement.t option
(** [as_atomic x] is [Some a] if [x] is [atomic a], and [None] otherwise. *)

val is_atomic : t -> bool
(** [is_atomic x] is [true] if [x] is [atomic a], and [false] otherwise. *)

val as_early_out : t -> Early_out.t option
(** [as_early_out x] is [Some e] if [x] is [early_out e], and [None]
    otherwise. *)

val as_label : t -> Act_common.C_id.t option
(** [as_label x] is [Some l] if [x] is [label l], and [None] otherwise. *)

val is_label : t -> bool
(** [is_label x] is [true] if [x] is [label l], and [false] otherwise. *)

(** Creates a basic monadic map over [M]. *)
module Base_map (M : Monad.S) : sig
  val bmap :
       t
    -> assign:(Assign.t -> Assign.t M.t)
    -> atomic:(Atomic_statement.t -> Atomic_statement.t M.t)
    -> early_out:(Early_out.t -> Early_out.t M.t)
    -> label:(Act_common.C_id.t -> Act_common.C_id.t M.t)
    -> goto:(Act_common.C_id.t -> Act_common.C_id.t M.t)
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
