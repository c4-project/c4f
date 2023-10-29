(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Atomic stores *)

open Import

(** An atomic store operation. *)
type t = {src: Expression.t; dst: Address.t; mo: Mem_order.t}
[@@deriving sexp, accessors, make, compare, equal]

(** {1 Traversals} *)

(** Traversing over atomic-action addresses in atomic stores. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t := t and type Elt.t = Address.t

(** Traversing over atomic-action expressions in atomic stores. *)
module On_expressions :
  Travesty.Traversable_types.S0
    with type t := t
     and type Elt.t = Expression.t

(** Traversing over memory orders in atomic stores.

    This traversal may silently fail to apply changes to the memory order if
    they result in a store-incompatible order. *)
module On_mem_orders :
  Travesty.Traversable_types.S0 with type t := t and type Elt.t = Mem_order.t

(** {1 Generating and quickchecking}

    There cannot be one single quickcheck instance for atomic stores, as the
    generation and shrinking processes are highly dependent on the specific
    sorts of expression and address that need to be generated. *)

val quickcheck_observer : t Base_quickcheck.Observer.t
(** [quickcheck_observer] is a generic observer for all atomic stores. *)

(** [Quickcheck_generic (Src) (Dst)] generates random stores, using [Src] to
    generate source expressions and [Dst] to generate destination addresses.
    It uses [Mem_order]'s store-compatible generator to pick random memory
    orders. *)
module Quickcheck_generic
    (_ : C4f_utils.My_quickcheck.S_with_sexp with type t := Expression.t)
    (_ : C4f_utils.My_quickcheck.S_with_sexp with type t := Address.t) :
  C4f_utils.My_quickcheck.S_with_sexp with type t = t
