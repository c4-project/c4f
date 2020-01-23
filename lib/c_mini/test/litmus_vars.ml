(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = Act_c_mini
  module Ac = Act_common
end

let%test_module "make_type_alist" =
  ( module struct
    (* This is a hand-translation of a Memalloy output (test '10' when using
       ACT's default Memalloy parameters, at time of writing). *)

    let init : (Ac.C_id.t, Src.Constant.t) List.Assoc.t =
      [ (Ac.C_id.of_string "x", Src.Constant.int 0)
      ; (Ac.C_id.of_string "y", Src.Constant.int 0) ]

    let postcondition : Src.Constant.t Act_litmus.Postcondition.t =
      Act_litmus.Postcondition.exists
        Act_litmus.Predicate.Infix.(
          Ac.Litmus_id.of_string "0:r0" ==? Src.Constant.int 1
          && Ac.Litmus_id.of_string "0:r1" ==? Src.Constant.int 0
          && Ac.Litmus_id.of_string "x" ==? Src.Constant.int 1
          && Ac.Litmus_id.of_string "y" ==? Src.Constant.int 1)

    let parameters : (Ac.C_id.t, Src.Type.t) List.Assoc.t =
      [ (Ac.C_id.of_string "x", Src.Type.int ~pointer:true ~atomic:true ())
      ; (Ac.C_id.of_string "y", Src.Type.int ~pointer:true ~atomic:true ())
      ]

    let p0_decls : (Ac.C_id.t, Src.Initialiser.t) List.Assoc.t =
      (* This ordering is intentional: not only is it how Memalloy generated
         this test, but it also checks to make sure the ordering of the
         associative list is correct. *)
      [ (Ac.C_id.of_string "r1", Src.Initialiser.of_int 0)
      ; (Ac.C_id.of_string "r0", Src.Initialiser.of_int 0) ]

    let p0_stms : unit Src.Statement.t list =
      [ Src.Statement.prim
          (Src.Prim_statement.assign
             (Src.Assign.make
                ~lvalue:(Src.Lvalue.of_variable_str_exn "r0")
                ~rvalue:
                  (Src.Expression.atomic_load
                     (Src.Atomic_load.make
                        ~src:(Src.Address.of_variable_str_exn "y")
                        ~mo:Src.Mem_order.Acquire))))
      ; Src.Statement.prim
          (Src.Prim_statement.assign
             (Src.Assign.make
                ~lvalue:(Src.Lvalue.of_variable_str_exn "r1")
                ~rvalue:
                  (Src.Expression.atomic_load
                     (Src.Atomic_load.make
                        ~src:(Src.Address.of_variable_str_exn "x")
                        ~mo:Src.Mem_order.Relaxed)))) ]

    let p0 : unit Src.Function.t =
      Src.Function.make ~body_decls:p0_decls ~body_stms:p0_stms ~parameters
        ()

    let p1_decls : (Ac.C_id.t, Src.Initialiser.t) List.Assoc.t = []

    let p1_stms : unit Src.Statement.t list =
      [ Src.Statement.prim
          (Src.Prim_statement.atomic_store ()
             (Src.Atomic_store.make
                ~src:(Src.Expression.int_lit 1)
                ~dst:(Src.Address.of_variable_str_exn "x")
                ~mo:Src.Mem_order.Relaxed))
      ; Src.Statement.prim
          (Src.Prim_statement.atomic_store ()
             (Src.Atomic_store.make
                ~src:(Src.Expression.int_lit 1)
                ~dst:(Src.Address.of_variable_str_exn "y")
                ~mo:Src.Mem_order.Release)) ]

    let p1 : unit Src.Function.t =
      Src.Function.make ~body_decls:p1_decls ~body_stms:p1_stms ~parameters
        ()

    let threads : unit Src.Function.t Ac.C_named.t list =
      [ Ac.C_named.make ~name:(Ac.C_id.of_string "P0") p0
      ; Ac.C_named.make ~name:(Ac.C_id.of_string "P1") p1 ]

    let header : Src.Constant.t Act_litmus.Header.t =
      Act_litmus.Header.make ~init ~postcondition ~name:"test_10" ()

    let test : Src.Litmus.Test.t =
      Or_error.ok_exn (Src.Litmus.Test.make ~header ~threads)

    let%expect_test "make_type_alist on example test" =
      Stdio.print_s
        [%sexp
          ( Src.Litmus_vars.make_type_alist test
            : (Ac.Litmus_id.t, Src.Type.t) List.Assoc.t Or_error.t )] ;
      [%expect
        {| (Ok ((x atomic_int*) (y atomic_int*) (0:r1 int) (0:r0 int))) |}]
  end )
