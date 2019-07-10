(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** An atomic load operation. *)

type t [@@deriving sexp, quickcheck, equal]

(** {2 Constructors} *)

val make : src:Address.t -> mo:Mem_order.t -> t
(** [atomic_load ~src ~dst ~mo] constructs an explicit atomic load
    expression with source [src] and memory order [mo]. *)

(** {2 Accessors} *)

val src : t -> Address.t
(** [src ld] gets [ld]'s source address. *)

val mo : t -> Mem_order.t
(** [mo ld] gets [ld]'s memory order. *)

include
  Types.S_has_underlying_variable with type t := t
(** We can get to the variable name inside an atomic load (that is, the
    source variable). *)

(** {2 Traversals} *)

(** Traversing over atomic-action addresses in atomic loads. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t

(** Traversing over lvalues in atomic loads. *)
module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t

(** {2 Generation and quickchecking}

    The main quickcheck instance for atomic loads generates any such loads,
    without regard to type safety over a particular typing environment. *)

module Quickcheck_atomic_ints (E : Env_types.S) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t
(** Generates random, type-safe atomic loads over the given variable typing
    environment, restricted to atomic ints. *)

(** {2 Type checking} *)

include
  Types.S_type_checkable with type t := t
(** Type-checking for atomic loads. *)
