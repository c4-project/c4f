(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-C: atomic statements (not expressions). *)

open Base

(** Opaque type of atomic statements.

    These don't carry metadata, as it's assumed that they'll be nested in a
    carrier type that does. *)
type t [@@deriving sexp, compare, equal]

(** {1 Constructors} *)

val cmpxchg : Expression.t Atomic_cmpxchg.t -> t
(** [cmpxchg a] lifts an atomic compare-exchange [a] to an atomic statement,
    discarding the boolean output. *)

val fence : Atomic_fence.t -> t
(** [fence a] lifts an atomic fence [a] to an atomic statement. *)

val fetch : Expression.t Atomic_fetch.t -> t
(** [fetch a] lifts an atomic fetch [a] to an atomic statement. *)

val store : Atomic_store.t -> t
(** [store a] lifts an atomic store [a] to an atomic statement. *)

(** {1 Traversals} *)

val reduce :
     t
  -> cmpxchg:(Expression.t Atomic_cmpxchg.t -> 'result)
  -> fence:(Atomic_fence.t -> 'result)
  -> fetch:(Expression.t Atomic_fetch.t -> 'result)
  -> store:(Atomic_store.t -> 'result)
  -> 'result
(** [reduce x ~atomic_cmpxchg ~atomic_fence ~atomic_store] reduces an atomic
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
    -> t Ap.t
  (** [bmap t ~cmpxchg ~fence ~store] traverses over [t] applicatively with
      the appropriate traversal function. *)
end

(** Traverses over the addresses of an atomic statement. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

(** Traverses over the expressions of an atomic statement. *)
module On_expressions :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Expression.t

(** Traverses over the lvalues of an atomic statement. *)
module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t
