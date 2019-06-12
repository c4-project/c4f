(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Mini = Act_c.Mini

let local (t : int) (id : Mini.Identifier.t) : Mini.Identifier.t =
  Act_common.Litmus_id.(to_memalloy_id (local t id))

let locals_in_statement (module T : Thread.S) : Mini.Statement.t -> Mini.Statement.t =
  Mini.Statement.On_identifiers.map
    ~f:(T.when_local ~over:Fn.id ~f:(local T.tid))


