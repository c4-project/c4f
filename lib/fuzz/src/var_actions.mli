(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer actions for manipulating variables. *)

(** Random state required for {{!Make_global} Make_global}. *)
module Global_payload : sig
  type t =
    { basic_type: Act_c_mini.Type.Basic.t
    ; initial_value: Act_c_mini.Constant.t
    ; name: Act_common.C_id.t }
  [@@deriving sexp]

  val generator : Var.Map.t -> t Base_quickcheck.Generator.t
  (** [generator vars] constructs a Quickcheck generator for payloads over
      the variable map [map]. *)
end

module Make_global : Action_types.S with type Payload.t = Global_payload.t
(** Fuzzer action that generates a new global variable. *)
