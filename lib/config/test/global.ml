(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio

open struct
  module Src = C4f_config
end

module Data = struct
  let fuzz : C4f_fuzz_run.Config.t Lazy.t =
    Lazy.from_fun C4f_fuzz_run.Config.make

  let global : Src.Global.t Lazy.t =
    Lazy.Let_syntax.(
      let%map fuzz = fuzz in
      Src.Global.make ~fuzz ())
end

let%test_module "accessors" =
  ( module struct
    let global = Lazy.force Data.global

    let%expect_test "fuzz" =
      print_s [%sexp (Src.Global.fuzz global : C4f_fuzz_run.Config.t)] ;
      [%expect {| ((weights ()) (params ()) (flags ())) |}]
  end )
