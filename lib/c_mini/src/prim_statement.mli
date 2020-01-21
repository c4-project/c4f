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

type 'meta t [@@deriving sexp, compare, equal]
(** Opaque type of mini-C primitive statements, parametrised by metadata. *)

(** {1 Constructors} *)

val assign : Assign.t -> 'meta t
(** [assign a] lifts an assignment [a] to a primitive statement. *)

val label : 'meta Label.t -> 'meta t
(** [label l] lifts a label [l] to a primitive statement. *)

val goto : 'meta Label.t -> 'meta t
(** [goto l] lifts a goto to label [l] to a primitive statement. *)

val nop : 'meta -> 'meta t
(** [nop m] creates a no-op (semicolon) primitive statement with metadata
    [m]. *)

val procedure_call : 'meta Call.t -> 'meta t
(** [procedure_call a] lifts a procedure (non-value-returning function) call
    [a] to a primitive statement. *)

(** {2 Atomics}

    See {!Atomic_statement}. *)

val atomic : 'meta -> Atomic_statement.t -> 'meta t
(** [atomic meta a] lifts an atomic statement [a], with metadata [meta], to a
    primitive statement. *)

val atomic_cmpxchg : 'meta -> Atomic_cmpxchg.t -> 'meta t
(** [atomic_cmpxchg meta a] lifts an atomic compare-exchange [a] to a
    primitive statement. *)

val atomic_fence : 'meta -> Atomic_fence.t -> 'meta t
(** [atomic_fence meta a] lifts an atomic fence [a] to a primitive statement. *)

val atomic_store : 'meta -> Atomic_store.t -> 'meta t
(** [atomic_store meta a] lifts an atomic store [a] to a primitive statement. *)

(** {2 Shorthand for early-out statements}

    See {!Early_out}. *)

val early_out : 'meta Early_out.t -> 'meta t
(** [early_out e] lifts an early-out statement [e] to a primitive statement. *)

val break : 'meta -> 'meta t
(** [break m] creates a break statement with metadata [m]. *)

val continue : 'meta -> 'meta t
(** [continue m] creates a continue statement with metadata [m]. *)

val return : 'meta -> 'meta t
(** [return m] creates a return statement with metadata [m]. *)

(** {1 Traversals} *)

val reduce :
     'meta t
  -> assign:((* 'meta *) Assign.t -> 'result)
  -> atomic:('meta * Atomic_statement.t -> 'result)
  -> early_out:('meta Early_out.t -> 'result)
  -> label:('meta Label.t -> 'result)
  -> goto:('meta Label.t -> 'result)
  -> nop:('meta -> 'result)
  -> procedure_call:('meta Call.t -> 'result)
  -> 'result
(** [reduce x ~assign ~atomic ~early_out ~nop] reduces a primitive statement
    [x] to a particular result type by applying the appropriate function. *)

(** Creates a basic monadic map over [M]. *)
module Base_map (M : Monad.S) : sig
  val bmap :
       'm1 t
    -> assign:((* 'm1 *) Assign.t -> (* 'm2 *) Assign.t M.t)
    -> atomic:('m1 * Atomic_statement.t -> ('m2 * Atomic_statement.t) M.t)
    -> early_out:('m1 Early_out.t -> 'm2 Early_out.t M.t)
    -> label:('m1 Label.t -> 'm2 Label.t M.t)
    -> goto:('m1 Label.t -> 'm2 Label.t M.t)
    -> nop:('m1 -> 'm2 M.t)
    -> procedure_call:('m1 Call.t -> 'm2 Call.t M.t)
    -> 'm2 t M.t
  (** [bmap x ~assign ~atomic ~early_out ~nop] maps the appropriate function
      over [x]. *)
end

module On_meta : Travesty.Traversable_types.S1 with type 'meta t := 'meta t
(** A traversal over the metadata in a primitive statement. *)

(** {2 Traversals with a fixed metadata type}

    To do traversals over parts of a primitive statement that aren't the
    metadata, we must fix the metadata in a functor. *)
module With_meta (Meta : T) : sig
  (** Traverses over the lvalues of a primitive expression. *)
  module On_lvalues :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Lvalue.t

  (** Traverses over the addresses of a primitive expression. *)
  module On_addresses :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Address.t
end
