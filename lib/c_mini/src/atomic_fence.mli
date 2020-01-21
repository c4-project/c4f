(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-C: atomic fences. *)

(** {1 Fence modes} *)

module Mode : sig
  (** Type of fence modes (thread, signal, etc.). *)
  type t =
    | Thread  (** 'atomic_thread_fence' *)
    | Signal  (** 'atomic_signal_fence' *)
  [@@deriving sexp, compare, equal]
end

(** {1 Fences themselves} *)

type t [@@deriving sexp, compare, equal]
(** Opaque type of atomic fences. *)

(** {2 Constructors} *)

val make : mode:Mode.t -> mo:Mem_order.t -> t
(** [make ~mode ~mo] makes a fence with the given mode and memory order. *)

(** {2 Accessors} *)

val mode : t -> Mode.t
(** [mode fence] gets [fence]'s mode. *)

val mo : t -> Mem_order.t
(** [mo fence] gets [fence]'s memory order. *)
