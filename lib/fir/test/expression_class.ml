(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = C4f_fir
end

let test_fragment : Src.Expression.t Lazy.t =
  lazy
    Src.(
      Expression.(
        eq
          (add (int_lit 27) (variable (C4f_common.C_id.of_string "foo")))
          (sub (int_lit 53)
             (atomic_fetch
                (Atomic_fetch.make
                   ~obj:(Address.of_variable_str_exn "bar")
                   ~arg:(int_lit 9) ~mo:Seq_cst ~op:`Add ) ) )))

let%test_module "matches_any" =
  ( module struct
    let test (templates : Src.Expression_class.t list) : unit =
      let k =
        Src.Expression_class.matches_any
          (Lazy.force test_fragment)
          ~templates
      in
      Stdio.printf "%b" k

    let%expect_test "no classes" = test [] ; [%expect {| false |}]

    let%expect_test "bop of any form" =
      test [Bop None] ;
      [%expect {| true |}]

    let%expect_test "bop subtract" =
      test [Bop (Some Src.Op.Binary.sub)] ;
      [%expect {| false |}]

    let%expect_test "bop eq" =
      test [Bop (Some Src.Op.Binary.eq)] ;
      [%expect {| true |}]

    let%expect_test "bop eq or sub" =
      test [Bop (Some Src.Op.Binary.eq); Bop (Some Src.Op.Binary.sub)] ;
      [%expect {| true |}]

    let%expect_test "uop" =
      test [Uop None] ;
      [%expect {| false |}]

    let%expect_test "constant" =
      test [Constant] ;
      [%expect {| false |}]

    let%expect_test "address" =
      test [Address] ;
      [%expect {| false |}]
  end )

let%test_module "unmatches_any" =
  ( module struct
    let test (templates : Src.Expression_class.t list) : unit =
      let k =
        Src.Expression_class.unmatches_any
          (Lazy.force test_fragment)
          ~templates
      in
      Stdio.printf "%b" k

    let%expect_test "no classes" = test [] ; [%expect {| false |}]

    let%expect_test "bop of any form" =
      test [Bop None] ;
      [%expect {| false |}]

    let%expect_test "bop subtract" =
      test [Bop (Some Src.Op.Binary.sub)] ;
      [%expect {| true |}]

    let%expect_test "bop eq" =
      test [Bop (Some Src.Op.Binary.eq)] ;
      [%expect {| false |}]

    let%expect_test "bop eq or sub" =
      test [Bop (Some Src.Op.Binary.eq); Bop (Some Src.Op.Binary.sub)] ;
      [%expect {| true |}]

    let%expect_test "uop" =
      test [Uop None] ;
      [%expect {| true |}]

    let%expect_test "constant" =
      test [Constant] ;
      [%expect {| true |}]

    let%expect_test "address" =
      test [Address] ;
      [%expect {| true |}]
  end )

let%test_module "rec_matches_any" =
  ( module struct
    let test (templates : Src.Expression_class.t list) : unit =
      let k =
        Src.Expression_class.rec_matches_any
          (Lazy.force test_fragment)
          ~templates
      in
      Stdio.printf "%b" k

    let%expect_test "no classes" = test [] ; [%expect {| false |}]

    let%expect_test "bop of any form" =
      test [Bop None] ;
      [%expect {| true |}]

    let%expect_test "bop subtract" =
      test [Bop (Some Src.Op.Binary.sub)] ;
      [%expect {| true |}]

    let%expect_test "bop eq" =
      test [Bop (Some Src.Op.Binary.eq)] ;
      [%expect {| true |}]

    let%expect_test "bop eq or sub" =
      test [Bop (Some Src.Op.Binary.eq); Bop (Some Src.Op.Binary.sub)] ;
      [%expect {| true |}]

    let%expect_test "uop" =
      test [Uop None] ;
      [%expect {| false |}]

    let%expect_test "constant" =
      test [Constant] ;
      [%expect {| true |}]

    let%expect_test "address" =
      test [Address] ;
      [%expect {| true |}]
  end )

let%test_module "rec_unmatches_any" =
  ( module struct
    let test (templates : Src.Expression_class.t list) : unit =
      let k =
        Src.Expression_class.rec_unmatches_any
          (Lazy.force test_fragment)
          ~templates
      in
      Stdio.printf "%b" k

    let%expect_test "no classes" = test [] ; [%expect {| false |}]

    let%expect_test "bop of any form" =
      test [Bop None] ;
      [%expect {| true |}]

    let%expect_test "bop subtract" =
      test [Bop (Some Src.Op.Binary.sub)] ;
      [%expect {| true |}]

    let%expect_test "bop eq" =
      test [Bop (Some Src.Op.Binary.eq)] ;
      [%expect {| true |}]

    let%expect_test "bop eq or sub" =
      test [Bop (Some Src.Op.Binary.eq); Bop (Some Src.Op.Binary.sub)] ;
      [%expect {| true |}]

    let%expect_test "uop" =
      test [Uop None] ;
      [%expect {| true |}]

    let%expect_test "constant" =
      test [Constant] ;
      [%expect {| true |}]

    let%expect_test "address" =
      test [Address] ;
      [%expect {| true |}]
  end )

let%test_module "count_matches" =
  ( module struct
    let test (templates : Src.Expression_class.t list) : unit =
      let k =
        Src.Expression_class.count_rec_matches
          (Lazy.force test_fragment)
          ~templates
      in
      Stdio.printf "%d" k

    let%expect_test "no classes" = test [] ; [%expect {| 0 |}]

    let%expect_test "bop of any form" =
      test [Bop None] ;
      [%expect {| 3 |}]

    let%expect_test "bop subtract" =
      test [Bop (Some Src.Op.Binary.sub)] ;
      [%expect {| 1 |}]

    let%expect_test "bop eq" =
      test [Bop (Some Src.Op.Binary.eq)] ;
      [%expect {| 1 |}]

    let%expect_test "bop eq or sub" =
      test [Bop (Some Src.Op.Binary.eq); Bop (Some Src.Op.Binary.sub)] ;
      [%expect {| 2 |}]

    let%expect_test "uop" =
      test [Uop None] ;
      [%expect {| 0 |}]

    let%expect_test "constant" =
      test [Constant] ;
      [%expect {| 3 |}]

    let%expect_test "address" =
      test [Address] ;
      [%expect {| 1 |}]
  end )
