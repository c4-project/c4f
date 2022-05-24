(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Enumeration of C11 memory orders. *)

open Base

type t =
  | Relaxed  (** [memory_order_relaxed] *)
  | Consume  (** [memory_order_consume] *)
  | Acquire  (** [memory_order_acquire] *)
  | Release  (** [memory_order_release] *)
  | Acq_rel  (** [memory_order_acq_rel] *)
  | Seq_cst  (** [memory_order_seq_cst] *)

include C4f_utils.Enum_types.S_table with type t := t

include C4f_utils.Enum_types.Extension_table with type t := t

(** {2 Predicates} *)

val is_load_compatible : t -> bool
(** [is_load_compatible mo] returns whether [mo] is compatible with load
    operations. *)

val is_store_compatible : t -> bool
(** [is_store_compatible mo] returns whether [mo] is compatible with store
    operations. *)

val is_cmpxchg_fail_compatible : t -> bool
(** [is_cmpxchg_fail_compatible mo] returns whether [mo] is compatible with
    the 'fail' order of a compare-exchange. *)

(** {2 Helpers for strengthening and weakening memory orders}

    Generally, ensuring memory order compatibility occurs at the individual
    statement/expression class level, and is baked into the various
    [On_mem_orders] traversals. *)

val can_change :
  t -> replacement:t -> direction:[< `Strengthen | `Weaken | `Any] -> bool
(** [can_change current ~replacement ~is_compatible ~direction] gets whether
    the memory order can be changed from [current] to [replacement] in the
    direction described by [direction]. *)

val try_change :
  t -> replacement:t -> direction:[< `Strengthen | `Weaken | `Any] -> t
(** [try_change current ~replacement ~direction] returns [replacement] if
    [can_change] is true, and [current] otherwise. *)

(** {2 Quickcheck support} *)

(** The Quickcheck implementation pulled in by [Extension_table], by default,
    generates from the whole pool of memory orders. To restrict to those that
    make sense in a particular context, see {{!gen_load} gen_load} et al. *)

val gen_load : t Base_quickcheck.Generator.t
(** [gen_load] generates a random memory order suitable for loads. *)

val gen_store : t Base_quickcheck.Generator.t
(** [gen_load] generates a random memory order suitable for stores. *)

val gen_cmpxchg_fail : t Base_quickcheck.Generator.t
(** [gen_cmpxchg_fail] generates a random memory order suitable for
    compare-exchange failures (assuming that the result is no stronger than
    the success memory order). *)
