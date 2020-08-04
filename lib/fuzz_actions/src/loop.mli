(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Actions that introduce, or rearrange, loops. *)

(** {1 Surround statements in do-while loops}

    This action removes a sublist of statements from the program, replacing
    them with a `do... while` statement containing some transformation of the
    removed statements.

    See also {!If_actions.Surround}. *)
module Surround :
  Act_fuzz.Action_types.S
    with type Payload.t = Act_fuzz.Payload.Cond_surround.t
