(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = Act_fir
end

let test_fragment : unit Src.Statement.t Lazy.t =
  lazy
    Src.(
      Statement.(
        if_stm
          (If.make ~cond:Expression.truth
             ~t_branch:
               (Block.of_statement_list
                  [ if_stm
                      (If.make ~cond:Expression.truth
                         ~t_branch:
                           (Block.of_statement_list
                              [ prim ()
                                  (Prim_statement.atomic_fetch
                                     (Atomic_fetch.make
                                        ~obj:
                                          (Address.of_variable_str_exn "x")
                                        ~arg:(Expression.int_lit 42)
                                        ~mo:Mem_order.Seq_cst
                                        ~op:Op.Fetch.Add)) ])
                         ~f_branch:
                           (Block.of_statement_list
                              [ prim ()
                                  (Prim_statement.atomic_store
                                     (Atomic_store.make
                                        ~dst:
                                          (Address.of_variable_str_exn "y")
                                        ~src:(Expression.int_lit 64)
                                        ~mo:Mem_order.Seq_cst)) ])) ])
             ~f_branch:
               (Block.of_statement_list
                  [ prim ()
                      (Prim_statement.atomic_xchg
                         (Atomic_xchg.make
                            ~obj:(Address.of_variable_str_exn "x")
                            ~desired:(Expression.int_lit 99)
                            ~mo:Mem_order.Seq_cst)) ]))))

let%test_module "count_matches" =
  ( module struct
    let test (template : Src.Statement_class.t) : unit =
      let k =
        Src.Statement_class.count_matches
          (Lazy.force test_fragment)
          ~template
      in
      Stdio.printf "%d" k

    let%expect_test "if statements" = test If ; [%expect {| 2 |}]

    let%expect_test "atomics of any form" =
      test (Src.Statement_class.atomic ()) ;
      [%expect {| 3 |}]

    let%expect_test "atomic_store" =
      test (Src.Statement_class.atomic ~specifically:Store ()) ;
      [%expect {| 1 |}]
  end )
