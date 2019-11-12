(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_c_mini

let cond : Src.Expression.t = Src.Expression.bool_lit true

let mkif (ts : unit Src.Statement.t list) (fs : unit Src.Statement.t list) :
    unit Src.Statement.t =
  Src.Statement.(
    if_stm
      (If.make ~cond
         ~t_branch:(Src.Block.of_statement_list ts)
         ~f_branch:(Src.Block.of_statement_list fs)))

let mkwhile (xs : unit Src.Statement.t list) : unit Src.Statement.t =
  Src.Statement.(
    while_loop
      (While.make ~cond ~kind:`While ~body:(Src.Block.of_statement_list xs)))

let%test_module "has_if_statements" =
  ( module struct
    let test (s : unit Src.Statement.t) : unit =
      Act_utils.Io.print_bool (Src.Statement.has_if_statements s)

    let%expect_test "nop" =
      test (Src.Statement.nop ()) ;
      [%expect {| false |}]

    let%expect_test "naked if statement" =
      test (mkif [] []) ;
      [%expect {| true |}]

    let%expect_test "while loop without if statement" =
      test (mkwhile []) ;
      [%expect {| false |}]

    let%expect_test "while loop with an if statement" =
      test (mkwhile [Src.Statement.nop (); mkif [] []; Src.Statement.nop ()]) ;
      [%expect {| true |}]
  end )
