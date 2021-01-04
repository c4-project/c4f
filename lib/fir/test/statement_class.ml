(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module A = Accessor
  module Src = C4f_fir
end

let test_fragment : unit Src.Statement.t Lazy.t =
  lazy
    Statement.(
      mkif ~cond:Src.Expression.truth
        [ mkif ~cond:Src.Expression.truth
            Src.
              [ mkafetch Add
                  Address.(of_variable_str_exn "x")
                  Expression.(int_lit 42)
              ; A.(
                  construct
                    Src.(Statement.prim' @> Prim_statement.label)
                    (C4f_common.C_id.of_string "foo")) ]
            Src.
              [ mkastore
                  Address.(of_variable_str_exn "y")
                  Expression.(int_lit 64) ] ]
        Src.
          [mkaxchg Address.(of_variable_str_exn "x") Expression.(int_lit 99)])

let%test_module "matches_any" =
  ( module struct
    let test (templates : Src.Statement_class.t list) : unit =
      let k =
        Src.Statement_class.matches_any (Lazy.force test_fragment) ~templates
      in
      Stdio.printf "%b" k

    let%expect_test "no classes" = test [] ; [%expect {| false |}]

    let%expect_test "if statements" =
      test [If] ;
      [%expect {| true |}]

    let%expect_test "labels" =
      test [Prim (Some Label)] ;
      [%expect {| false |}]

    let%expect_test "while loops" =
      test [Src.Statement_class.while_loop ()] ;
      [%expect {| false |}]

    let%expect_test "atomics of any form" =
      test [Src.Statement_class.atomic ()] ;
      [%expect {| false |}]

    let%expect_test "atomic_store" =
      test [Src.Statement_class.atomic ~specifically:Store ()] ;
      [%expect {| false |}]
  end )

let%test_module "unmatches_any" =
  ( module struct
    let test (templates : Src.Statement_class.t list) : unit =
      let k =
        Src.Statement_class.unmatches_any
          (Lazy.force test_fragment)
          ~templates
      in
      Stdio.printf "%b" k

    let%expect_test "no classes" = test [] ; [%expect {| false |}]

    let%expect_test "if statements" =
      test [If] ;
      [%expect {| false |}]

    let%expect_test "labels" =
      test [Prim (Some Label)] ;
      [%expect {| true |}]

    let%expect_test "while loops" =
      test [Src.Statement_class.while_loop ()] ;
      [%expect {| true |}]

    let%expect_test "atomics of any form" =
      test [Src.Statement_class.atomic ()] ;
      [%expect {| true |}]

    let%expect_test "atomic_store" =
      test [Src.Statement_class.atomic ~specifically:Store ()] ;
      [%expect {| true |}]
  end )

let%test_module "rec_matches_any" =
  ( module struct
    let test (templates : Src.Statement_class.t list) : unit =
      let k =
        Src.Statement_class.rec_matches_any
          (Lazy.force test_fragment)
          ~templates
      in
      Stdio.printf "%b" k

    let%expect_test "no classes" = test [] ; [%expect {| false |}]

    let%expect_test "if statements" =
      test [If] ;
      [%expect {| true |}]

    let%expect_test "while loops" =
      test [Src.Statement_class.while_loop ()] ;
      [%expect {| false |}]

    let%expect_test "atomics of any form" =
      test [Src.Statement_class.atomic ()] ;
      [%expect {| true |}]

    let%expect_test "atomic_store" =
      test [Src.Statement_class.atomic ~specifically:Store ()] ;
      [%expect {| true |}]
  end )

let%test_module "rec_unmatches_any" =
  ( module struct
    let test (templates : Src.Statement_class.t list) : unit =
      let k =
        Src.Statement_class.rec_unmatches_any
          (Lazy.force test_fragment)
          ~templates
      in
      Stdio.printf "%b" k

    let%expect_test "no classes" = test [] ; [%expect {| false |}]

    let%expect_test "if statements" =
      test [If] ;
      [%expect {| true |}]

    let%expect_test "while loops" =
      test [Src.Statement_class.while_loop ()] ;
      [%expect {| true |}]

    let%expect_test "atomics of any form" =
      test [Src.Statement_class.atomic ()] ;
      [%expect {| true |}]

    let%expect_test "atomic_store" =
      test [Src.Statement_class.atomic ~specifically:Store ()] ;
      [%expect {| true |}]
  end )

let%test_module "count_matches" =
  ( module struct
    let test (templates : Src.Statement_class.t list) : unit =
      let k =
        Src.Statement_class.count_rec_matches
          (Lazy.force test_fragment)
          ~templates
      in
      Stdio.printf "%d" k

    let%expect_test "if statements" =
      test [If] ;
      [%expect {| 2 |}]

    let%expect_test "atomics of any form" =
      test [Src.Statement_class.atomic ()] ;
      [%expect {| 3 |}]

    let%expect_test "atomic_store" =
      test [Src.Statement_class.atomic ~specifically:Store ()] ;
      [%expect {| 1 |}]

    let%expect_test "atomic_store or atomic_load" =
      test
        [ Src.Statement_class.atomic ~specifically:Store ()
        ; Src.Statement_class.atomic ~specifically:Load () ] ;
      [%expect {| 1 |}]
  end )
