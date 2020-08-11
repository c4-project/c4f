(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = Act_litmus_c
end

let%test_module "sift_decls" =
  ( module struct
    let%expect_test "sift_decls: mixed example" =
      let result =
        Or_error.(
          [`Decl "foo"; `Decl "bar"; `Ndecl "baz"; `Ndecl "barbaz"]
          |> Src.Abstract_prim.sift_decls
          >>| fun (x, y) ->
          (x, List.map y ~f:(function `Decl _ -> "DECL" | `Ndecl x -> x)))
      in
      Stdio.print_s [%sexp (result : (string list * string list) Or_error.t)] ;
      [%expect {| (Ok ((foo bar) (baz barbaz))) |}]
  end )
