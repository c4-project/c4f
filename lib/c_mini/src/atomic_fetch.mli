(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(** Fetch-style read-modify-write operations. *)

(** Opaque type of fetches.

    Fetches take the expression type as a type parameter. This is necessary
    to bust a dependency cycle when fetches appear in expression position.

    Fetches can be quickchecked, given appropriate expression endpoints.
    While this quickcheck generates appropriate memory orders, it will use
    random addresses, and is unsuitable for type-safe/environment-safe
    generation. *)
type 'e t [@@deriving sexp, compare, equal, quickcheck]

(** {1 Constructors} *)

val make : obj:Address.t -> arg:'e -> mo:Mem_order.t -> op:Op.Fetch.t -> 'e t
(** [make ~obj ~arg ~mo ~op] makes a fetch-and-X operation, whereby X is
    given by [op]; [obj] is the address of the atomic object to fetch; [arg]
    is the argument to [op]; and [mo] the memory order. *)

(** {1 Accessors} *)

val obj : _ t -> Address.t
(** [obj f] gets the address of the atomic object [f] is fetching. *)

val arg : 'e t -> 'e
(** [arg f] gets the argument to [f]'s operation. *)

val mo : _ t -> Mem_order.t
(** [mo f] gets [f]'s memory order. *)

val op : _ t -> Op.Fetch.t
(** [op f] gets [f]'s postfix operation. *)

(** Primitive building block for atomic-fetch quickchecks. *)
module Quickcheck_generic
    (A : Act_utils.My_quickcheck.S_with_sexp with type t := Address.t)
    (O : Act_utils.My_quickcheck.S_with_sexp with type t := Op.Fetch.t)
    (E : Act_utils.My_quickcheck.S_with_sexp) : sig
  type nonrec t = E.t t [@@deriving sexp_of, quickcheck]
end

(** {1 Traversal primitives} *)

(** Primitive building block for producing traversals over atomic fetches.

    For module recursion reasons, we don't provide actual traversals here;
    see {!Expression}. *)
module Base_map (Ap : Applicative.S) : sig
  val bmap :
       'a t
    -> obj:(Address.t -> Address.t Ap.t)
    -> arg:('a -> 'b Ap.t)
    -> mo:(Mem_order.t -> Mem_order.t Ap.t)
    -> op:(Op.Fetch.t -> Op.Fetch.t Ap.t)
    -> 'b t Ap.t
end

(** {1 Interface implementations} *)

include Expression_types.S_atomic with type 'e t := 'e t
