(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions that generate store instructions. *)

(** {1 Random state} *)

module Random_state : sig
  type t

  val make : store:Act_c_mini.Atomic_store.t -> path:Path.Program.t -> t

  val store : t -> Act_c_mini.Atomic_store.t

  val path : t -> Path.Program.t
end

(** {1 Action modules} *)

module Int : Action_types.S with type Payload.t = Random_state.t
(** [Int] is a fuzzer action that generates a random atomic-int store
    instruction. *)
