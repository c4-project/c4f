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
  (** Opaque type of fence payloads. *)
  type t [@@deriving sexp]

  val make : path:Act_fuzz.Path.t -> fence:Act_fir.Atomic_fence.t -> t
  (** [make ~path ~fence] constructs a fence payload with path [path] and
      fence [fence]. *)
end

(** {2 Action} *)

(** [Fence] is an action that inserts random memory fences. *)
module Fence :
  Act_fuzz.Action_types.S
    with type Payload.t =
          Act_fir.Atomic_fence.t Act_fuzz.Payload_impl.Insertion.t

(** {1 Memory order strengthening} *)

(** {2 Payload} *)

module Strengthen_payload : sig
  (** Opaque type of MO-strengthening payloads. *)
  type t [@@deriving sexp]

  val make :
    path:Act_fuzz.Path.t -> mo:Act_fir.Mem_order.t -> can_weaken:bool -> t
  (** [make ~path ~mo ~can_weaken] constructs a strengthening payload with
      path [path], memory order [mo], and whether or not weakening is
      allowed. *)
end

(** {2 Action} *)

(** [Strengthen] is an action that tries to strengthen memory orders. *)
module Strengthen :
  Act_fuzz.Action_types.S with type Payload.t = Strengthen_payload.t
