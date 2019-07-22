(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Qualifying local identifiers by their thread IDs.

    When operating the delitmusifier in 'make all variables global' mode, it
    needs to flatten out thread-local variables in the global scope. This
    module contains the logic for doing so. *)

open Base

(** {2 Qualifying identifiers in Litmus constructs}

    These functions involve thread-qualifying thread-local litmus IDs in
    litmus constructs. Generally, these contain the requisite thread IDs
    in-place, meaning we needn't carry around a thread context. *)

val litmus_id : Act_common.Litmus_id.t -> Act_common.C_id.t
(** [litmus_id id] qualifies a litmus identifier [id], using any thread
    information therein. *)

val postcondition :
     Act_c_mini.Constant.t Act_litmus.Postcondition.t
  -> Act_c_mini.Constant.t Act_litmus.Postcondition.t
(** [locals_in_statement thread statement] qualifies all local variables in
    the mini-statement [statement] using the information in [thread]. *)

(** {2 Qualifying identifiers in C constructs}

    These functions involve thread-qualifying C IDs in thread bodies.
    Generally, these do _not_ contain self-starting thread context, and so
    we need to carry around increasing amounts of auxiliary information. *)

val local : int -> Act_common.C_id.t -> Act_common.C_id.t
(** [local tid id] qualifies a local variable [id] using the thread
    identifier [tid]. *)
