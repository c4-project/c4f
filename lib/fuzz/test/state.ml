(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Test_data = struct
  (* TODO(@MattWindsor91): sort out the discrepancy between the subject
     example and var map. *)

  let state : Src.State.t Lazy.t =
    Lazy.Let_syntax.(
      let%map vars = Var.Test_data.test_map in
      Src.State.make ~vars ())
end
