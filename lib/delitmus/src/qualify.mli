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

val litmus_id :
  ?qualify_locals:bool -> Act_common.Litmus_id.t -> Act_common.C_id.t
(** [litmus_id ?qualify_locals id] qualifies a litmus identifier [id], using
    any thread information therein. *)

val postcondition :
     ?qualify_locals:bool
  -> Act_fir.Constant.t Act_litmus.Postcondition.t
  -> Act_fir.Constant.t Act_litmus.Postcondition.t
(** [postcondition ?qualify_locals pc] qualifies all identifiers in the
    postcondition [pc]. *)
