(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_fuzz

module Test_data = struct
  let init : Act_c_mini.Constant.t Act_common.C_named.Alist.t Lazy.t =
    lazy
      [ (Act_common.C_id.of_string "x", Act_c_mini.Constant.int 27)
      ; (Act_common.C_id.of_string "y", Act_c_mini.Constant.int 53) ]

  let globals : Act_c_mini.Type.t Act_common.C_named.Alist.t Lazy.t =
    lazy
      Act_c_mini.
        [ ( Act_common.C_id.of_string "x"
          , Type.(int ~pointer:true ~atomic:true ()) )
        ; ( Act_common.C_id.of_string "y"
          , Type.(int ~pointer:true ~atomic:true ()) ) ]

  let state : Act_fuzz.State.t Lazy.t =
    (* TODO(@MattWindsor91): labels? *)
    let labels = Set.empty (module Src.Subject.Label) in
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
      Act_fuzz.State.make ~vars ~labels ())

  let sample_known_true_if : Src.Subject.Statement.t Lazy.t =
    lazy
      Act_c_mini.(
        Statement.(
          if_stm
            (If.make
               ~cond:
                 Expression.(
                   Infix.(
                     of_variable_str_exn "foo" == of_variable_str_exn "y"))
               ~t_branch:
                 (Src.Subject.Block.make_generated
                    ~statements:
                      [ atomic_store
                          (Atomic_store.make ~src:(Expression.int_lit 56)
                             ~dst:(Address.of_variable_str_exn "x")
                             ~mo:Mem_order.Seq_cst) ]
                    ())
               ~f_branch:(Src.Subject.Block.make_dead_code ()))))

  let sample_known_false_if : Src.Subject.Statement.t Lazy.t =
    lazy
      Act_c_mini.(
        Statement.(
          if_stm
            (If.make ~cond:Expression.falsehood
               ~t_branch:
                 (Src.Subject.Block.make_dead_code
                    ~statements:
                      [ atomic_store
                          (Atomic_store.make ~src:(Expression.int_lit 95)
                             ~dst:(Address.of_variable_str_exn "y")
                             ~mo:Mem_order.Seq_cst) ]
                    ())
               ~f_branch:(Src.Subject.Block.make_generated ()))))

  let sample_once_do_while : Src.Subject.Statement.t Lazy.t =
    lazy
      Act_c_mini.(
        Statement.(
          while_loop
            (While.make
               ~cond:Expression.(Infix.(int_lit 4 == int_lit 5))
               ~body:
                 (Src.Subject.Block.make_dead_code
                    ~statements:
                      [ atomic_store
                          (Atomic_store.make ~src:(Expression.int_lit 44)
                             ~dst:(Address.of_variable_str_exn "x")
                             ~mo:Mem_order.Seq_cst) ]
                    ())
               ~kind:`Do_while)))

  let body_stms : Src.Subject.Statement.t list Lazy.t =
    Lazy.Let_syntax.(
      let%map sample_known_true_if = sample_known_true_if
      and sample_known_false_if = sample_known_false_if
      and sample_once_do_while = sample_once_do_while in
      Act_c_mini.(
        Statement.
          [ atomic_store
              (Atomic_store.make ~src:(Expression.int_lit 42)
                 ~dst:(Address.of_variable_str_exn "x")
                 ~mo:Mem_order.Seq_cst)
          ; nop Src.Metadata.generated
          ; atomic_store
              (Atomic_store.make
                 ~src:(Expression.of_variable_str_exn "foo")
                 ~dst:(Address.of_variable_str_exn "y")
                 ~mo:Mem_order.Relaxed)
          ; sample_known_true_if
          ; sample_known_false_if
          ; sample_once_do_while ]))

  let threads : Src.Subject.Thread.t list Lazy.t =
    Lazy.Let_syntax.(
      let%map stms = body_stms in
      [Src.Subject.Thread.make ~stms ()])

  let test : Src.Subject.Test.t Lazy.t =
    Lazy.Let_syntax.(
      let%bind init = init in
      let%map threads = threads in
      let header = Act_litmus.Header.make ~name:"example" ~init () in
      Act_litmus.Test.Raw.make ~header ~threads)
end

let%test_module "using sample environment" =
  ( module struct
    type r = Act_c_mini.Litmus.Lang.Program.t list [@@deriving sexp_of]

    let env = Lazy.force Act_c_mini_test.Env.test_env

    let vars : Src.Var.Map.t =
      env
      |> Act_utils.My_map.map_with_keys
           (module Act_common.Litmus_id)
           ~f:(fun ~key ~data ->
             let lit = Act_common.Litmus_id.global key in
             (lit, Src.Var.Record.make_existing Global data))
      |> Or_error.ok_exn |> Act_common.Scoped_map.of_litmus_id_map

    let test programs =
      let res = Src.Subject.Thread.list_to_litmus programs ~vars in
      Fmt.(
        pr "@[<v>%a@]@." (list ~sep:sp Act_c_mini.Litmus.Lang.Program.pp) res)

    let%expect_test "list_to_litmus: empty test" = test [] ; [%expect {| |}]

    let%expect_test "list_to_litmus: empty threads" =
      test (List.init 5 ~f:(fun _ -> Src.Subject.Thread.empty)) ;
      [%expect {| |}]

    let%expect_test "list_to_litmus: sample threads" =
      test (Lazy.force Test_data.threads) ;
      [%expect
        {|
        void
        P0(atomic_int *bar, bool barbaz, int *blep, int foo, atomic_bool *foobaz,
           atomic_int x, atomic_int y, atomic_bool z)
        {
            atomic_store_explicit(x, 42, memory_order_seq_cst);
            ;
            atomic_store_explicit(y, foo, memory_order_relaxed);
            if (foo == y) { atomic_store_explicit(x, 56, memory_order_seq_cst); }
            if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
            do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
            5);
        } |}]

    let%expect_test "list_to_litmus: sample threads with interspersed \
                     emptiness" =
      let threads =
        Test_data.threads |> Lazy.force
        |> List.intersperse ~sep:Src.Subject.Thread.empty
      in
      test threads ;
      [%expect
        {|
        void
        P0(atomic_int *bar, bool barbaz, int *blep, int foo, atomic_bool *foobaz,
           atomic_int x, atomic_int y, atomic_bool z)
        {
            atomic_store_explicit(x, 42, memory_order_seq_cst);
            ;
            atomic_store_explicit(y, foo, memory_order_relaxed);
            if (foo == y) { atomic_store_explicit(x, 56, memory_order_seq_cst); }
            if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
            do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
            5);
        } |}]
  end )
