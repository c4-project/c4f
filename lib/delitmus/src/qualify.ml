(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(* Note: These functions don't yet make sure that they're generating fresh
   (ie not globally shadowed) names. Eventually, they should all take an
   optional set of global identifiers, and should try some munging if they
   generate clashes. *)

let litmus_id ?(qualify_locals : bool = true) (id : Common.Litmus_id.t) :
    Common.C_id.t =
  ( if qualify_locals then Common.Litmus_id.to_memalloy_id
  else Common.Litmus_id.variable_name )
    id

let postcondition ?(qualify_locals : bool = true)
    (pc : Fir.Constant.t Litmus.Postcondition.t) :
    Fir.Constant.t Litmus.Postcondition.t =
  (* We can't just map over [litmus_id ~qualify_locals], as it'll lose the
     thread IDs in the postcondition. *)
  (* TODO(@MattWindsor91): perhaps don't qualify things we've already
   * qualified. *)
  if qualify_locals then
    Litmus.Postcondition.map_left pc
      ~f:(Fn.compose Common.Litmus_id.global litmus_id)
  else pc
