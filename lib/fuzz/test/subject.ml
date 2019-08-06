(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
open Act_fuzz
open Subject

module Example = struct
  let init : Act_c_mini.Constant.t Act_c_mini.Named.Alist.t Lazy.t =
    lazy
      [ (Act_common.C_id.of_string "x", Act_c_mini.Constant.int 27)
      ; (Act_common.C_id.of_string "y", Act_c_mini.Constant.int 53) ]

  let globals : Act_c_mini.Type.t Act_c_mini.Named.Alist.t Lazy.t =
    lazy
      Act_c_mini.
        [ ( Act_common.C_id.of_string "x"
          , Type.(int ~pointer:true ~atomic:true ()) )
        ; ( Act_common.C_id.of_string "y"
          , Type.(int ~pointer:true ~atomic:true ()) ) ]

  let state : Act_fuzz.State.t Lazy.t =
    Lazy.Let_syntax.(
      let%map globals_alist = globals in
      let vars =
        globals_alist
        |> List.map ~f:(fun (id, ty) ->
               ( Act_common.Litmus_id.global id
               , Act_fuzz.Var.Record.make_existing Global ty ))
        |> Map.of_alist_exn (module Act_common.Litmus_id)
        |> Act_common.Scoped_map.of_litmus_id_map
      in
      Act_fuzz.State.make ~vars ())

  let body_stms : Act_c_mini.Statement.t list Lazy.t =
    lazy
      Act_c_mini.
        [ Statement.atomic_store
            (Atomic_store.make
               ~src:(Expression.constant (Act_c_mini.Constant.int 42))
               ~dst:(Address.of_variable (Act_common.C_id.of_string "x"))
               ~mo:Mem_order.Seq_cst)
        ; Statement.nop ()
        ; Statement.atomic_store
            (Atomic_store.make
               ~src:
                 (Expression.lvalue
                    (Lvalue.variable (Act_common.C_id.of_string "foo")))
               ~dst:(Address.of_variable (Act_common.C_id.of_string "y"))
               ~mo:Mem_order.Relaxed) ]

  let programs : Subject.Program.t list Lazy.t =
    Lazy.Let_syntax.(
      let%bind parameters = globals in
      let%map body_stms = body_stms in
      Act_c_mini.
        [ Subject.Program.of_function
            (Function.make ~parameters ~body_decls:[] ~body_stms ()) ])

  let test : Subject.Test.t Lazy.t =
    Lazy.Let_syntax.(
      let%bind init = init in
      let%map threads = programs in
      let aux = Act_litmus.Aux.make ~init () in
      Act_litmus.Test.Raw.make ~name:"example" ~aux ~threads)
end

let%test_module "program" =
  ( module struct
    open Program

    let%expect_test "empty program has no statements" =
      Fmt.(pr "%a@." (using has_statements bool) empty) ;
      [%expect {| false |}]
  end )

let%test_module "using sample environment" =
  ( module struct
    type r = Act_c_mini.Litmus.Lang.Program.t list [@@deriving sexp_of]

    let env = Lazy.force Act_c_mini_test.Env.test_env

    let vars : Var.Map.t =
      env
      |> Act_utils.My_map.map_with_keys
           (module Act_common.Litmus_id)
           ~f:(fun ~key ~data ->
             let lit = Act_common.Litmus_id.global key in
             (lit, Var.Record.make_existing Global data))
      |> Or_error.ok_exn |> Act_common.Scoped_map.of_litmus_id_map

    let run programs =
      let result = Program.list_to_litmus programs ~vars in
      print_s [%sexp (result : r)]

    let%expect_test "programs_to_litmus: empty test" =
      run [] ; [%expect {| () |}]

    let%expect_test "programs_to_litmus: empty programs" =
      run (List.init 5 ~f:(fun _ -> Program.empty)) ;
      [%expect {| () |}]

    let%expect_test "programs_to_litmus: sample programs" =
      run (Lazy.force Example.programs) ;
      [%expect
        {|
        (((name P0)
          (value
           ((parameters
             ((bar atomic_int*) (barbaz bool) (blep int*) (foo int)
              (foobaz atomic_bool*) (x atomic_int) (y atomic_int) (z atomic_bool)))
            (body_decls ())
            (body_stms
             ((Atomic_store
               ((src (Constant (Int 42))) (dst (Lvalue (Variable x)))
                (mo memory_order_seq_cst)))
              Nop
              (Atomic_store
               ((src (Lvalue (Variable foo))) (dst (Lvalue (Variable y)))
                (mo memory_order_relaxed))))))))) |}]

    let%expect_test "programs_to_litmus: sample programs with interspersed \
                     emptiness" =
      let programs =
        Example.programs |> Lazy.force
        |> List.intersperse ~sep:Program.empty
      in
      run programs ;
      [%expect
        {|
        (((name P0)
          (value
           ((parameters
             ((bar atomic_int*) (barbaz bool) (blep int*) (foo int)
              (foobaz atomic_bool*) (x atomic_int) (y atomic_int) (z atomic_bool)))
            (body_decls ())
            (body_stms
             ((Atomic_store
               ((src (Constant (Int 42))) (dst (Lvalue (Variable x)))
                (mo memory_order_seq_cst)))
              Nop
              (Atomic_store
               ((src (Lvalue (Variable foo))) (dst (Lvalue (Variable y)))
                (mo memory_order_relaxed))))))))) |}]
  end )
