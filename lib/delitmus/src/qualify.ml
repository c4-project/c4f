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

let litmus_id (id : Act_common.Litmus_id.t) : Act_c.Mini.Identifier.t =
  Act_common.Litmus_id.to_memalloy_id id

let local (t : int) (id : Act_c.Mini.Identifier.t) : Act_c.Mini.Identifier.t
    =
  litmus_id (Act_common.Litmus_id.local t id)

let locals_in_statement (module T : Thread.S) :
    Act_c.Mini.Statement.t -> Act_c.Mini.Statement.t =
  Act_c.Mini.Statement.On_identifiers.map
    ~f:(T.when_local ~over:Fn.id ~f:(local T.tid))

let postcondition
    (pc : Act_c.Mini.Constant.t Act_litmus.Ast_base.Postcondition.t) :
    Act_c.Mini.Constant.t Act_litmus.Ast_base.Postcondition.t =
  let module M =
    Act_litmus.Ast_base.Postcondition.On_identifiers (Act_c.Mini.Constant) in
  (* TODO(@MattWindsor91): perhaps don't qualify things we've already
   * qualified. *)
  M.map pc ~f:(Fn.compose Act_common.Litmus_id.global litmus_id)
