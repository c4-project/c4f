(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer flow actions that generate control flows in dead code.

    See also {!If_actions}. *)

(** {1 Early-out} *)

module Early_out_payload : sig
  type t
  (** Opaque type of early-out payloads. *)

  val make : path:Path.Program.t -> kind:Act_c_mini.Early_out.t -> t
  (** [make ~path ~kind] constructs an early-out payload with the path [path]
      and early-out kind [kind]. *)
end

module Early_out : Action_types.S with type Payload.t = Early_out_payload.t
(** A fuzzer action that inserts early-out (break, return, etc) statements
    into dead-code. *)

(** {1 Goto} *)

module Goto_payload : sig
  type t
  (** Opaque type of goto payloads. *)

  val make : path:Path.Program.t -> label:Act_common.C_id.t -> t
  (** [make ~path ~label] constructs a goto payload with the path [path] and
      label [label] (relative to the thread of [path]).. *)
end

module Goto : Action_types.S with type Payload.t = Goto_payload.t
(** A fuzzer action that inserts goto statements to random labels into
    dead-code. *)
