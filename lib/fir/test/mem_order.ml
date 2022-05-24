(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = C4f_fir
end

(* Testing to make sure that various strengthening comparisons we expect to
   hold between memory orders do hold. *)
let%test_module "comparisons" =
  ( module struct
    let test (l : Src.Mem_order.t) (r : Src.Mem_order.t) : unit =
      C4f_utils.Io.print_bool Src.Mem_order.(l < r)

    let%expect_test "rlx not stronger than sc" =
      test Src.Mem_order.Seq_cst Src.Mem_order.Relaxed ;
      [%expect {| false |}]

    let%expect_test "sc stronger than rlx" =
      test Src.Mem_order.Relaxed Src.Mem_order.Seq_cst ;
      [%expect {| true |}]

    let%expect_test "sc stronger than acq" =
      test Src.Mem_order.Acq_rel Src.Mem_order.Seq_cst ;
      [%expect {| true |}]

    let%expect_test "sc stronger than rel" =
      test Src.Mem_order.Release Src.Mem_order.Seq_cst ;
      [%expect {| true |}]

    let%expect_test "acqrel stronger than acq" =
      test Src.Mem_order.Acquire Src.Mem_order.Acq_rel ;
      [%expect {| true |}]

    let%expect_test "acqrel stronger than rel" =
      test Src.Mem_order.Release Src.Mem_order.Acq_rel ;
      [%expect {| true |}]
  end )

let%test_module "can_change" =
  ( module struct
    let%expect_test "can change rlx to sc if strengthening" =
      C4f_utils.Io.print_bool
        Src.Mem_order.(
          can_change Relaxed ~replacement:Seq_cst ~direction:`Strengthen) ;
      [%expect {| true |}]

    let%expect_test "cannot change rlx to sc if weakening" =
      C4f_utils.Io.print_bool
        Src.Mem_order.(
          can_change Relaxed ~replacement:Seq_cst ~direction:`Weaken) ;
      [%expect {| false |}]

    let%expect_test "can change rlx to sc if no direction" =
      C4f_utils.Io.print_bool
        Src.Mem_order.(
          can_change Relaxed ~replacement:Seq_cst ~direction:`Any) ;
      [%expect {| true |}]

    let%expect_test "cannot change sc to rlx if strengthening" =
      C4f_utils.Io.print_bool
        Src.Mem_order.(
          can_change Seq_cst ~replacement:Relaxed ~direction:`Strengthen) ;
      [%expect {| false |}]

    let%expect_test "can change sc to rlx if weakening" =
      C4f_utils.Io.print_bool
        Src.Mem_order.(
          can_change Seq_cst ~replacement:Relaxed ~direction:`Weaken) ;
      [%expect {| true |}]

    let%expect_test "can change sc to rlx if no direction" =
      C4f_utils.Io.print_bool
        Src.Mem_order.(
          can_change Seq_cst ~replacement:Relaxed ~direction:`Any) ;
      [%expect {| true |}]
  end )
