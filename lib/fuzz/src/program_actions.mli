(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer actions for manipulating programs. *)

module Make_empty : Action_types.S with type Payload.t = unit
(** Fuzzer action that generates a new, empty program. *)

module Label_payload : sig
  type t [@@deriving sexp]
  (** Opaque type of label payloads. *)

  val make : path:Path.Program.t -> name:Act_common.C_id.t -> t
  (** [make ~path ~name] makes a label payload with path [path] and label
      name [name]. *)
end

module Label : Action_types.S with type Payload.t = Label_payload.t
(** Fuzzer action that inserts a new, random label into the program. *)
