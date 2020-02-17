(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-C: atomic statements (not expressions). *)

open Base

type t [@@deriving sexp, compare, equal]
(** Opaque type of atomic statements.

    These don't carry metadata, as it's assumed that they'll be nested in a
    carrier type that does. *)

(** {1 Constructors} *)

val atomic_fence : Atomic_fence.t -> t
(** [atomic_fence a] lifts an atomic fence [a] to an atomic statement. *)

val atomic_store : Atomic_store.t -> t
(** [atomic_store a] lifts an atomic store [a] to an atomic statement. *)

val atomic_cmpxchg : Atomic_cmpxchg.t -> t
(** [atomic_cmpxchg a] lifts an atomic compare-exchange [a] to an atomic
    statement. *)

(** {1 Traversals} *)

val reduce :
     t
  -> atomic_cmpxchg:(Atomic_cmpxchg.t -> 'result)
  -> atomic_fence:(Atomic_fence.t -> 'result)
  -> atomic_store:(Atomic_store.t -> 'result)
  -> 'result
(** [reduce x ~atomic_cmpxchg ~atomic_fence ~atomic_store] reduces an atomic
    statement [x] to a particular result type by applying the appropriate
    function. *)

(** Traverses over the addresses of an atomic statement. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

(** Traverses over the expressions of an atomic statement. *)
module On_expressions :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Expression.t

(** Traverses over the lvalues of an atomic statement. *)
module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t
