(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** An atomic load operation. *)

open Import

(** Type of atomic load operations. *)
type t = {src: Address.t; mo: Mem_order.t}
[@@deriving sexp, accessors, compare, equal, quickcheck]

(** {2 Constructors} *)

val make : src:Address.t -> mo:Mem_order.t -> t
(** [atomic_load ~src ~dst ~mo] constructs an explicit atomic load expression
    with source [src] and memory order [mo]. *)

(** {2 Accessors} *)

(** We can get to the variable name inside an atomic load (that is, the
    source variable). *)
include Types.S_has_underlying_variable with type t := t

(** {2 Traversals} *)

(** Traversing over atomic-action addresses in atomic loads. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

(** Traversing over memory orders in atomic loads.

    This traversal silently rejects any mappings that cause the memory order
    to become load-incompatible. *)
module On_mem_orders :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Mem_order.t

(** {2 Generation and quickchecking}

    The main quickcheck instance for atomic loads generates any such loads,
    without regard to type safety over a particular typing environment.

    For more interesting quickcheck behaviour, see {!C4f_fir_gen}. *)

(** Generic building block for making custom quickcheck generators. *)
module Quickcheck_generic
    (_ : C4f_utils.My_quickcheck.S_with_sexp with type t := Address.t) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end

(** {2 Type checking} *)

(** Type-checking for atomic loads. *)
include Types.S_type_checkable with type t := t
