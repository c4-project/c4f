(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions relating to memory operations and changes to memory order. *)

(** {1 Fence insertion} *)

(** {2 Payload} *)

module Fence_payload : sig
  type t [@@deriving sexp]
  (** Opaque type of fence payloads. *)

  val make :
       path:Path.Program.t
    -> fence:Act_c_mini.Atomic_fence.t
    -> t
  (** [make ~path ~fence] constructs a fence payload with path [path]
      and fence [fence]. *)
end

(** {2 Action} *)

module Fence : Action_types.S with type Payload.t = Fence_payload.t
(** [Fence] is an action that inserts random memory fences. *)
