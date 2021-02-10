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

let cond : Src.Expression.t = Src.Expression.bool_lit true

let mkif ?(cond : Src.Expression.t = cond) (ts : unit Src.Statement.t list)
    (fs : unit Src.Statement.t list) : unit Src.Statement.t =
  A.construct Src.Statement.if_stm
    (Src.If.make ~cond
       ~t_branch:(Src.Block.of_statement_list ts)
       ~f_branch:(Src.Block.of_statement_list fs))

let mkwhile ?(cond : Src.Expression.t = cond)
    (xs : unit Src.Statement.t list) : unit Src.Statement.t =
  A.construct Src.Statement.flow
    (Src.Flow_block.while_loop ~cond ~kind:While
       ~body:(Src.Block.of_statement_list xs))

let nop : unit Src.Statement.t =
  A.(construct Src.(Statement.prim' @> Prim_statement.nop) ())

let mkastore ?(mo : Src.Mem_order.t = Seq_cst) (dst : Src.Address.t)
    (src : Src.Expression.t) : unit Src.Statement.t =
  A.(
    construct
      Src.(
        Statement.prim' @> Prim_statement.atomic @> Atomic_statement.store))
    (Src.Atomic_store.make ~mo ~dst ~src)

let mkafetch ?(mo : Src.Mem_order.t = Seq_cst) (op : Src.Op.Fetch.t)
    (obj : Src.Address.t) (arg : Src.Expression.t) : unit Src.Statement.t =
  A.(
    construct
      Src.(
        Statement.prim' @> Prim_statement.atomic @> Atomic_statement.fetch))
    (Src.Atomic_fetch.make ~mo ~obj ~arg ~op)

let mkaxchg ?(mo : Src.Mem_order.t option) (obj : Src.Address.t)
    (arg : Src.Expression.t) : unit Src.Statement.t =
  mkafetch ?mo `Xchg obj arg

let%test_module "has_if_statements" =
  ( module struct
    let test (s : unit Src.Statement.t) : unit =
      C4f_utils.Io.print_bool (Src.Statement.has_if_statements s)

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
