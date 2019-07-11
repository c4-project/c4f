(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer actions for manipulating variables. *)

(** Random state required for {{!Make_global} Make_global}. *)
module Global_random_state : sig
  type t = {is_atomic: bool; initial_value: int; name: Act_common.C_id.t}
  [@@deriving sexp]
end

module Make_global :
  Action_types.S with type Random_state.t = Global_random_state.t
(** Fuzzer action that generates a new global variable. *)
