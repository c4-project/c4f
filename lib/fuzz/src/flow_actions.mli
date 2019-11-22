(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Fuzzer flow actions that haven't been further categorised.

    See also {!If_actions}. *)

module Dead_return : Action_types.S with type Payload.t = Path.program
(** A fuzzer action that inserts return statements into dead-code. *)
