(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_fir

let cond : Src.Expression.t = Src.Expression.bool_lit true

let mkif ?(cond : Src.Expression.t = cond) (ts : unit Src.Statement.t list)
    (fs : unit Src.Statement.t list) : unit Src.Statement.t =
  Src.Statement.if_stm
    (Src.If.make ~cond
       ~t_branch:(Src.Block.of_statement_list ts)
       ~f_branch:(Src.Block.of_statement_list fs))

let mkwhile ?(cond : Src.Expression.t = cond)
    (xs : unit Src.Statement.t list) : unit Src.Statement.t =
  Src.Statement.flow
    (Src.Flow_block.while_loop ~cond ~kind:While
       ~body:(Src.Block.of_statement_list xs))

let prim : Src.Prim_statement.t -> unit Src.Statement.t =
  Src.Statement.prim ()

let nop : unit Src.Statement.t = prim Src.Prim_statement.nop

let mkaxchg ?(mo : Src.Mem_order.t = Seq_cst) (obj : Src.Address.t)
    (desired : Src.Expression.t) : unit Src.Statement.t =
  prim
    (Src.Prim_statement.atomic_xchg (Src.Atomic_xchg.make ~mo ~obj ~desired))

let mkastore ?(mo : Src.Mem_order.t = Seq_cst) (dst : Src.Address.t)
    (src : Src.Expression.t) : unit Src.Statement.t =
  prim
    (Src.Prim_statement.atomic_store (Src.Atomic_store.make ~mo ~dst ~src))

let mkafetch ?(mo : Src.Mem_order.t = Seq_cst) (op : Src.Op.Fetch.t)
    (obj : Src.Address.t) (arg : Src.Expression.t) : unit Src.Statement.t =
  prim
    (Src.Prim_statement.atomic_fetch
       (Src.Atomic_fetch.make ~mo ~obj ~arg ~op))

let%test_module "has_if_statements" =
  ( module struct
    let test (s : unit Src.Statement.t) : unit =
      Act_utils.Io.print_bool (Src.Statement.has_if_statements s)

    let%expect_test "nop" = test nop ; [%expect {| false |}]

    let%expect_test "naked if statement" =
      test (mkif [] []) ;
      [%expect {| true |}]

    let%expect_test "while loop without if statement" =
      test (mkwhile []) ;
      [%expect {| false |}]

    let%expect_test "while loop with an if statement" =
      test (mkwhile [nop; mkif [] []; nop]) ;
      [%expect {| true |}]
  end )
