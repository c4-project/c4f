(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** An atomic store operation. *)

type t [@@deriving sexp, compare, equal]

(** {1 Constructors} *)

val make : src:Expression.t -> dst:Address.t -> mo:Mem_order.t -> t
(** [atomic_store ~src ~dst ~mo] constructs an explicit atomic store
    expression with source [src], destination [dst], and memory order [mo]. *)

(** {1 Accessors} *)

val dst : t -> Address.t
(** [dst st] gets [st]'s destination address. *)

val src : t -> Expression.t
(** [src st] gets [st]'s source expression. *)

val mo : t -> Mem_order.t
(** [mo st] gets [st]'s memory order. *)

(** {1 Traversals} *)

(** Traversing over atomic-action addresses in atomic stores. *)
module On_addresses :
  Travesty.Traversable_types.S0 with type t := t and type Elt.t = Address.t

(** Traversing over lvalues in atomic stores. *)
module On_lvalues :
  Travesty.Traversable_types.S0 with type t := t and type Elt.t = Lvalue.t

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
    (Src : Act_utils.My_quickcheck.S_with_sexp with type t := Expression.t)
    (Dst : Act_utils.My_quickcheck.S_with_sexp with type t := Address.t) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t

(** There isn't a generic quickcheck instance for atomic stores, as we can't
    guarantee type safety in general. *)

(** [Quickcheck_ints (E)] generates random stores from atomic integers to
    non-atomic integers, using [Src] as the variable typing environment for
    sources and [Dst] as the environment for destinations. *)
module Quickcheck_ints (Src : Env_types.S) (Dst : Env_types.S) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t
