(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions that generate store instructions. *)

(** {1 Keys of relevant parameters and flags} *)

val forbid_already_written_flag_key : Act_common.Id.t
(** [forbid_already_written_flag_key] is the key used to look-up the
    forbid-already-written flag. *)

(** {1 Payload} *)

module Store_payload : sig
  type t

  val make : store:Act_c_mini.Atomic_store.t -> path:Path.Program.t -> t

  val store : t -> Act_c_mini.Atomic_store.t

  val path : t -> Path.Program.t
end

(** {1 Action modules} *)

module Int : Action_types.S with type Payload.t = Store_payload.t
(** [Int] is a fuzzer action that generates a random atomic-int store
    instruction. *)

module Int_dead : Action_types.S with type Payload.t = Store_payload.t
(** [Int_dead] is a variant of [Int] that only targets dead-code, and, in
    turn, requires and adds fewer constraints on the destination. *)

module Int_redundant : Action_types.S with type Payload.t = Store_payload.t
(** [Int_redundant] is a variant of [Int] that only stores a destination's
    known value back to itself. *)
