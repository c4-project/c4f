(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(* Needed because Base shadows it: *)
module Ty = Type
open Base

(** Opaque type of atomic expressions. *)
type 'e t [@@deriving sexp, compare, equal, quickcheck]

(** {1 Constructors} *)

val cmpxchg : 'e Atomic_cmpxchg.t -> 'e t
(** [cmpxchg c] lifts [c] to an atomic expression. *)

val fetch : 'e Atomic_fetch.t -> 'e t
(** [fetch f] lifts the fetch (or exchange) [f] to an atomic expression. *)

val load : Atomic_load.t -> 'e t
(** [load l] lifts [l] to an atomic expression. *)

(** {1 Traversals and reductions} *)

val reduce :
     'e t
  -> cmpxchg:('e Atomic_cmpxchg.t -> 'a)
  -> fetch:('e Atomic_fetch.t -> 'a)
  -> load:(Atomic_load.t -> 'a)
  -> 'a
(** [reduce ae ~cmpxchg ~fetch ~load] applies the correct function to [ae]
    depending on its type. *)

(** Primitive mapper for applicative traversal. *)
module Base_map (Ap : Applicative.S) : sig
  val bmap :
       'a t
    -> cmpxchg:('a Atomic_cmpxchg.t -> 'b Atomic_cmpxchg.t Ap.t)
    -> fetch:('a Atomic_fetch.t -> 'b Atomic_fetch.t Ap.t)
    -> load:(Atomic_load.t -> Atomic_load.t Ap.t)
    -> 'b t Ap.t
  (** [bmap ae ~cmpxchg ~fetch ~load] applies the correct function to [ae]
      depending on its type, each function serving as an applicative
      traversal of its recipient. *)
end

(** [On_expressions] permits traversing over the expressions inside an atomic
    expression. *)
module On_expressions : Travesty.Traversable_types.S1 with type 'e t = 'e t

(** {1 Type checking} *)

(** We can type check atomic expressions, so long as we already type-checked
    any recursive expressions inside them. *)
include Types.S_type_checkable with type t := Ty.t t
