(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(* Note: These functions don't yet make sure that they're generating fresh
   (ie not globally shadowed) names. Eventually, they should all take an
   optional set of global identifiers, and should try some munging if they
   generate clashes. *)

let litmus_id (id : Act_common.Litmus_id.t) : Act_common.C_id.t =
  Act_common.Litmus_id.to_memalloy_id id

let local (t : int) (id : Act_common.C_id.t) : Act_common.C_id.t =
  litmus_id (Act_common.Litmus_id.local t id)

let postcondition (pc : Act_c_mini.Constant.t Act_litmus.Postcondition.t) :
    Act_c_mini.Constant.t Act_litmus.Postcondition.t =
  (* TODO(@MattWindsor91): perhaps don't qualify things we've already
   * qualified. *)
  Act_litmus.Postcondition.map_left pc
    ~f:(Fn.compose Act_common.Litmus_id.global litmus_id)
