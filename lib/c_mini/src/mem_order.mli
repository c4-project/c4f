(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Enumeration of C11 memory orders. *)

open Base

type t =
  | Seq_cst  (** [memory_order_seq_cst] *)
  | Release  (** [memory_order_release] *)
  | Acquire  (** [memory_order_acquire] *)
  | Acq_rel  (** [memory_order_acq_rel] *)
  | Relaxed  (** [memory_order_relaxed] *)
  | Consume  (** [memory_order_consume] *)

include Act_utils.Enum_types.S_table with type t := t

include Act_utils.Enum_types.Extension_table with type t := t

(** {2 Predicates} *)

val is_load_compatible : t -> bool
(** [is_load_compatible mo] returns whether [mo] is compatible with load
    operations. *)

val is_store_compatible : t -> bool
(** [is_store_compatible mo] returns whether [mo] is compatible with store
    operations. *)

val is_rmw_compatible : t -> bool
(** [is_rmw_compatible mo] returns whether [mo] is compatible with
    read-modify-writes. *)

(** {2 Quickcheck support} *)

(** The Quickcheck implementation pulled in by [Extension_table], by default,
    generates from the whole pool of memory orders. To restrict to those that
    make sense in a particular context, see {{!gen_load} gen_load} et al. *)

val gen_load : t Base_quickcheck.Generator.t
(** [gen_load] generates a random memory order suitable for loads. *)

val gen_store : t Base_quickcheck.Generator.t
(** [gen_load] generates a random memory order suitable for stores. *)

val gen_rmw : t Base_quickcheck.Generator.t
(** [gen_load] generates a random memory order suitable for
    read-modify-writes. *)
