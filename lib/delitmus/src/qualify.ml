(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let local (t : int) (id : Act_c.Mini.Identifier.t) : Act_c.Mini.Identifier.t =
  Act_common.Litmus_id.(to_memalloy_id (local t id))

let locals_in_statement (module T : Thread.S) : Act_c.Mini.Statement.t -> Act_c.Mini.Statement.t =
  Act_c.Mini.Statement.On_identifiers.map
    ~f:(T.when_local ~over:Fn.id ~f:(local T.tid))


