(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: atomic fences. *)

(** {1 Fence modes} *)

module Mode : sig
  (** Type of fence modes (thread, signal, etc.). *)
  type t =
    | Thread  (** 'atomic_thread_fence' *)
    | Signal  (** 'atomic_signal_fence' *)

  include C4f_utils.Enum_types.Extension_table with type t := t
end

(** {1 Fences themselves} *)

(** Opaque type of atomic fences. *)
type t [@@deriving sexp, compare, equal, quickcheck]

(** {2 Constructors} *)

val make : mode:Mode.t -> mo:Mem_order.t -> t
(** [make ~mode ~mo] makes a fence with the given mode and memory order. *)

(** {2 Accessors} *)

val mode : t -> Mode.t
(** [mode fence] gets [fence]'s mode. *)

val mo : t -> Mem_order.t
(** [mo fence] gets [fence]'s memory order. *)

(** {2 Traversals} *)

(** Traverses over the memory order of an atomic fence. *)
module On_mem_orders :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Mem_order.t
