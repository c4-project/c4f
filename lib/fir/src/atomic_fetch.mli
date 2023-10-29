(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fetch-style read-modify-write operations. *)

open Base
open Import

(** The fetch record itself.

    Fetches take the expression type as a type parameter. This is necessary
    to bust a dependency cycle when fetches appear in expression position.

    Fetches can be quickchecked, given appropriate expression endpoints.
    While this quickcheck generates appropriate memory orders, it will use
    random addresses, and is unsuitable for type-safe/environment-safe
    generation. *)
type 'e t = {obj: Address.t; arg: 'e; mo: Mem_order.t; op: Op.Fetch.t}
[@@deriving sexp, accessors, compare, equal, quickcheck]

(** {1 Constructors} *)

val make : obj:Address.t -> arg:'e -> mo:Mem_order.t -> op:Op.Fetch.t -> 'e t
(** [make ~obj ~arg ~mo ~op] makes a fetch-and-X operation, whereby X is
    given by [op]; [obj] is the address of the atomic object to fetch; [arg]
    is the argument to [op]; and [mo] the memory order. *)

(** {1 Traversal primitives} *)

val variable_of : ('i, Common.C_id.t, 'e t, [< field]) Accessor.t
(** [variable_of] accesses the underlying object variable of the fetch. It
    does not access any other variables. *)

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

(** Primitive building block for atomic-fetch quickchecks. *)
module Quickcheck_generic
    (_ : C4f_utils.My_quickcheck.S_with_sexp with type t := Address.t)
    (_ : C4f_utils.My_quickcheck.S_with_sexp with type t := Op.Fetch.t)
    (E : C4f_utils.My_quickcheck.S_with_sexp) : sig
  type nonrec t = E.t t [@@deriving sexp_of, quickcheck]
end

(** {1 Interface implementations} *)

include Expression_types.S_atomic with type 'e t := 'e t
