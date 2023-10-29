(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let%test_module "make_alist" =
  ( module struct
    (* This is a hand-translation of a Memalloy output (test '10' when using
       ACT's default Memalloy parameters, at time of writing). *)

    let init : (Common.C_id.t, Src.Constant.t) List.Assoc.t =
      [ (Common.C_id.of_string "x", Src.Constant.int 0)
      ; (Common.C_id.of_string "y", Src.Constant.int 0) ]

    let postcondition : Src.Constant.t C4f_litmus.Postcondition.t =
      C4f_litmus.Postcondition.exists
        C4f_litmus.Predicate.Infix.(
          Common.Litmus_id.of_string "0:r0" ==? Src.Constant.int 1
          && Common.Litmus_id.of_string "0:r1" ==? Src.Constant.int 0
          && Common.Litmus_id.of_string "x" ==? Src.Constant.int 1
          && Common.Litmus_id.of_string "y" ==? Src.Constant.int 1 )

    let parameters : (Common.C_id.t, Src.Type.t) List.Assoc.t =
      [ ( Common.C_id.of_string "x"
        , Src.Type.int ~is_pointer:true ~is_atomic:true () )
      ; ( Common.C_id.of_string "y"
        , Src.Type.int ~is_pointer:true ~is_atomic:true () ) ]

    let p0_decls : (Common.C_id.t, Src.Initialiser.t) List.Assoc.t =
      (* This ordering is intentional: not only is it how Memalloy generated
         this test, but it also checks to make sure the ordering of the
         associative list is correct. *)
      [ (Common.C_id.of_string "r1", Src.Initialiser.of_int 0)
      ; (Common.C_id.of_string "r0", Src.Initialiser.of_int 0) ]

    let p0_stms : unit Src.Statement.t list =
      [ Src.(
          Accessor.construct
            (Statement.prim' @> Prim_statement.assign)
            Assign.(
              Lvalue.of_variable_str_exn "r0"
              @= Expression.atomic_load
                   (Atomic_load.make
                      ~src:(Address.of_variable_str_exn "y")
                      ~mo:Acquire ) ) )
      ; Src.(
          Accessor.construct
            (Statement.prim' @> Prim_statement.assign)
            Assign.(
              Lvalue.of_variable_str_exn "r1"
              @= Expression.atomic_load
                   (Atomic_load.make
                      ~src:(Address.of_variable_str_exn "x")
                      ~mo:Relaxed ) ) ) ]

    let p0 : unit Src.Function.t =
      Src.Function.make ~body_decls:p0_decls ~body_stms:p0_stms ~parameters
        ()

    let p1_decls : (Common.C_id.t, Src.Initialiser.t) List.Assoc.t = []

    let p1_stms : unit Src.Statement.t list =
      [ Accessor.construct
          Src.(
            Statement.prim' @> Prim_statement.atomic
            @> Atomic_statement.store )
          (Src.Atomic_store.make
             ~src:(Src.Expression.int_lit 1)
             ~dst:(Src.Address.of_variable_str_exn "x")
             ~mo:Src.Mem_order.Relaxed )
      ; Accessor.construct
          Src.(
            Statement.prim' @> Prim_statement.atomic
            @> Atomic_statement.store )
          (Src.Atomic_store.make
             ~src:(Src.Expression.int_lit 1)
             ~dst:(Src.Address.of_variable_str_exn "y")
             ~mo:Src.Mem_order.Release ) ]

    let p1 : unit Src.Function.t =
      Src.Function.make ~body_decls:p1_decls ~body_stms:p1_stms ~parameters
        ()

    let threads : unit Src.Function.t Common.C_named.t list =
      [ Common.C_named.make ~name:(Common.C_id.of_string "P0") p0
      ; Common.C_named.make ~name:(Common.C_id.of_string "P1") p1 ]

    let header : Src.Constant.t C4f_litmus.Header.t =
      C4f_litmus.Header.make ~init ~postcondition ~name:"test_10" ()

    let test : Src.Litmus.Test.t =
      Or_error.ok_exn (Src.Litmus.Test.make ~header ~threads)

    let%expect_test "make_alist on example test" =
      Stdio.print_s
        [%sexp
          ( Src.Litmus.Var.make_alist test
            : (Common.Litmus_id.t, Src.Litmus.Var.Record.t) List.Assoc.t
              Or_error.t )] ;
      [%expect
        {|
          (Ok
           ((x ((ty atomic_int*) (param_index 0) (initial_value ((Int 0)))))
            (y ((ty atomic_int*) (param_index 1) (initial_value ((Int 0)))))
            (0:r1 ((ty int) (param_index 2) (initial_value ((Int 0)))))
            (0:r0 ((ty int) (param_index 3) (initial_value ((Int 0))))))) |}]
  end )
