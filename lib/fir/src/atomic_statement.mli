(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: atomic statements (not expressions). *)

open Base

(** Opaque type of atomic statements.

    These don't carry metadata, as it's assumed that they'll be nested in a
    carrier type that does. *)
type t [@@deriving sexp, compare, equal]

(** {1 Accessors} *)

val cmpxchg :
  ( 'a
  , Expression.t Atomic_cmpxchg.t
  , t
  , [< Accessor.variant] )
  Accessor.Simple.t
(** [cmpxchg] focuses on atomic compare-exchanges that are in statement
    position (discarding the boolean output). *)

val fence : ('a, Atomic_fence.t, t, [< Accessor.variant]) Accessor.Simple.t
(** [fence] focuses on atomic fence statements. *)

val fetch :
  ( 'a
  , Expression.t Atomic_fetch.t
  , t
  , [< Accessor.variant] )
  Accessor.Simple.t
(** [fetch] focuses on atomic fetch statements. *)

val store : ('a, Atomic_store.t, t, [< Accessor.variant]) Accessor.Simple.t
(** [store] focuses on atomic store statements. *)

val xchg :
  ('a, Expression.t Atomic_xchg.t, t, [< Accessor.variant]) Accessor.Simple.t
(** [xchg] focuses on atomic exchanges that are in statement position
    (discarding the output). *)

(** {1 Traversals} *)

val value_map :
     t
  -> cmpxchg:(Expression.t Atomic_cmpxchg.t -> 'result)
  -> fence:(Atomic_fence.t -> 'result)
  -> fetch:(Expression.t Atomic_fetch.t -> 'result)
  -> store:(Atomic_store.t -> 'result)
  -> xchg:(Expression.t Atomic_xchg.t -> 'result)
  -> 'result
(** [value_map x ~cmpxchg ~fence ~fetch ~store ~xchg] reduces an atomic
    statement [x] to a particular result type by applying the appropriate
    function. *)

(** [Base_map] is the base form of an applicative traversal. *)
module Base_map (Ap : Applicative.S) : sig
  val bmap :
       t
    -> cmpxchg:
         (   Expression.t Atomic_cmpxchg.t
          -> Expression.t Atomic_cmpxchg.t Ap.t)
    -> fence:(Atomic_fence.t -> Atomic_fence.t Ap.t)
    -> fetch:
         (Expression.t Atomic_fetch.t -> Expression.t Atomic_fetch.t Ap.t)
    -> store:(Atomic_store.t -> Atomic_store.t Ap.t)
    -> xchg:(Expression.t Atomic_xchg.t -> Expression.t Atomic_xchg.t Ap.t)
    -> t Ap.t
  (** [bmap t ~cmpxchg ~fence ~fetch ~store ~xchg] traverses over [t]
      applicatively with the appropriate traversal function. *)
end

(** Traverses over the addresses of an atomic statement. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

(** Traverses over the expressions of an atomic statement. *)
module On_expressions :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Expression.t

(** Traverses over the memory orders of an atomic statement. *)
module On_mem_orders :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Mem_order.t
