(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Import

(** Qualifying local identifiers by their thread IDs.

    When operating the delitmusifier in 'make all variables global' mode, it
    needs to flatten out thread-local variables in the global scope. This
    module contains the logic for doing so. *)

val litmus_id : ?qualify_locals:bool -> Common.Litmus_id.t -> Common.C_id.t
(** [litmus_id ?qualify_locals id] qualifies a litmus identifier [id], using
    any thread information therein. *)

val postcondition :
     ?qualify_locals:bool
  -> Fir.Constant.t Litmus.Postcondition.t
  -> Fir.Constant.t Litmus.Postcondition.t
(** [postcondition ?qualify_locals pc] qualifies all identifiers in the
    postcondition [pc]. *)
