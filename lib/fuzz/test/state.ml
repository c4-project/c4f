(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
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
      Src.State.make ~vars () )
end
