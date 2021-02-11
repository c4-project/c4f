(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Test_data = struct
  let init : Fir.Constant.t C4f_common.C_named.Alist.t Lazy.t =
    lazy
      [ (C4f_common.C_id.of_string "x", Fir.Constant.int 27)
      ; (C4f_common.C_id.of_string "y", Fir.Constant.int 53) ]

  let body_decls : Fir.Initialiser.t C4f_common.C_named.Alist.t Lazy.t =
    lazy
      [ ( C4f_common.C_id.of_string "r0"
        , Fir.
            { Initialiser.ty= Fir.Type.int ~is_atomic:true ()
            ; value= Fir.Constant.int 4004 } )
      ; ( C4f_common.C_id.of_string "r1"
        , Fir.
            { Initialiser.ty= Fir.Type.int ~is_atomic:false ()
            ; value= Fir.Constant.int 8008 } ) ]

  let mk_store (a : Fir.Atomic_store.t) : Src.Subject.Statement.t =
    Src.Subject.Statement.make_generated_prim
      (Accessor.construct
         Fir.(Prim_statement.atomic @> Atomic_statement.store)
         a )

  let mk_always_true_if (cond : Fir.Expression.t)
      (t : Src.Subject.Statement.t list) (f : Src.Subject.Statement.t list) :
      Src.Subject.Statement.t =
    Fir.(
      Accessor.construct Statement.if_stm
        (If.make ~cond
           ~t_branch:(Src.Subject.Block.make_generated ~statements:t ())
           ~f_branch:(Src.Subject.Block.make_dead_code ~statements:f ()) ))

  let sample_known_true_if : Src.Subject.Statement.t Lazy.t =
    lazy
      Fir.(
        mk_always_true_if
          Expression.(
            Infix.(of_variable_str_exn "foo" == of_variable_str_exn "y"))
          [ mk_store
              (Atomic_store.make ~src:(Expression.int_lit 56)
                 ~dst:(Address.of_variable_str_exn "x")
                 ~mo:Mem_order.Seq_cst )
          ; Src.Subject.Statement.make_generated_prim
              Fir.(
                Accessor.construct Prim_statement.label
                  (C4f_common.C_id.of_string "kappa_kappa")) ]
          [])

  let sample_known_false_if : Src.Subject.Statement.t Lazy.t =
    lazy
      Fir.(
        Accessor.construct Statement.if_stm
          (If.make ~cond:Expression.falsehood
             ~t_branch:
               (Src.Subject.Block.make_dead_code
                  ~statements:
                    [ mk_store
                        (Atomic_store.make
                           ~src:
                             (Expression.atomic_load
                                (Atomic_load.make
                                   ~src:(Address.of_variable_str_exn "x")
                                   ~mo:Mem_order.Seq_cst ) )
                           ~dst:(Address.of_variable_str_exn "y")
                           ~mo:Mem_order.Seq_cst ) ]
                  () )
             ~f_branch:(Src.Subject.Block.make_generated ()) ))

  let sample_once_do_while : Src.Subject.Statement.t Lazy.t =
    lazy
      Fir.(
        Accessor.construct Statement.flow
          (Flow_block.while_loop
             ~cond:Expression.(Infix.(int_lit 4 == int_lit 5))
             ~body:
               (Block.make ~metadata:Src.Metadata.gen_once
                  ~statements:
                    [ mk_store
                        (Atomic_store.make ~src:(Expression.int_lit 44)
                           ~dst:(Address.of_variable_str_exn "x")
                           ~mo:Mem_order.Seq_cst ) ]
                  () )
             ~kind:Do_while ))

  let sample_dead_while : Src.Subject.Statement.t Lazy.t =
    lazy
      Fir.(
        Accessor.construct Statement.flow
          (Flow_block.while_loop
             ~cond:Expression.(Infix.(int_lit 4 == int_lit 5))
             ~body:
               (Src.Subject.Block.make_dead_code
                  ~statements:
                    [ mk_store
                        (Atomic_store.make ~src:(Expression.int_lit 44)
                           ~dst:(Address.of_variable_str_exn "x")
                           ~mo:Mem_order.Seq_cst ) ]
                  () )
             ~kind:While ))

  let sample_multi_for : Src.Subject.Statement.t Lazy.t =
    lazy
      Fir.(
        Accessor.construct Statement.flow
          (Flow_block.for_loop_simple
             ~control:
               { lvalue= Lvalue.of_variable_str_exn "r1"
               ; init_value= Expression.int_lit 0
               ; cmp_value= Expression.int_lit 2
               ; direction= Up Inclusive }
             (Src.Subject.Block.make_generated
                ~statements:
                  [ mk_store
                      (Atomic_store.make ~src:(Expression.int_lit 99)
                         ~dst:(Address.of_variable_str_exn "x")
                         ~mo:Mem_order.Seq_cst ) ]
                () ) ))

  let body_stms : Src.Subject.Statement.t list Lazy.t =
    Lazy.Let_syntax.(
      let%map sample_known_true_if = sample_known_true_if
      and sample_known_false_if = sample_known_false_if
      and sample_once_do_while = sample_once_do_while
      and sample_multi_for = sample_multi_for
      and sample_dead_while = sample_dead_while in
      Fir.
        [ mk_store
            (Atomic_store.make ~src:(Expression.int_lit 42)
               ~dst:(Address.of_variable_str_exn "x")
               ~mo:Mem_order.Seq_cst )
        ; Src.Subject.Statement.make_generated_prim
            (Accessor.construct Prim_statement.nop ())
        ; mk_store
            (Atomic_store.make
               ~src:(Expression.of_variable_str_exn "foo")
               ~dst:(Address.of_variable_str_exn "y")
               ~mo:Mem_order.Relaxed )
        ; sample_known_true_if
        ; sample_known_false_if
        ; sample_once_do_while
        ; sample_multi_for
        ; sample_dead_while ])

  let pos_0_first_atom = 0

  let pos_0_nop = 1

  let pos_0_last_atom = 2

  let pos_0_true_if = 3

  let pos_0_false_if = 4

  let pos_0_once_do_while = 5

  let pos_0_multi_for = 6

  let pos_0_dead_while = 7

  let thread0 : Src.Subject.Thread.t Lazy.t =
    Lazy.Let_syntax.(
      let%map stms = body_stms and decls = body_decls in
      Src.Subject.Thread.make ~stms ~decls ())

  let thread1_stms : Src.Subject.Statement.t list Lazy.t =
    lazy
      Fir.
        [ Src.Subject.Statement.make_generated_prim
            (Accessor.construct Prim_statement.label
               (C4f_common.C_id.of_string "loop") )
        ; mk_always_true_if Fir.Expression.truth []
            [ Src.Subject.Statement.make_generated_prim
                (Accessor.construct Prim_statement.goto
                   (C4f_common.C_id.of_string "loop") ) ] ]

  let thread1 : Src.Subject.Thread.t Lazy.t =
    Lazy.map ~f:(fun stms -> Src.Subject.Thread.make ~stms ()) thread1_stms

  let threads : Src.Subject.Thread.t list Lazy.t =
    Lazy.Let_syntax.(
      let%map t0 = thread0 and t1 = thread1 in
      [t0; t1])

  let test : Src.Subject.Test.t Lazy.t =
    Lazy.Let_syntax.(
      let%bind init = init in
      let%map threads = threads in
      let header = C4f_litmus.Header.make ~name:"example" ~init () in
      C4f_litmus.Test.Raw.make ~header ~threads)

  module Path = struct
    (* These will need manually synchronising with the statements above. *)

    let flag (fs : Src.Path_meta.Flag.t list) (p : Src.Path.t Lazy.t) :
        Src.Path.With_meta.t Lazy.t =
      Lazy.map p ~f:(fun path ->
          Src.Path_meta.(
            let anchor = None (* for now *) in
            let meta = {flags= Set.of_list (module Flag) fs; anchor} in
            With_meta.make path ~meta) )

    let thread_0_stms (path : Src.Path.Stms.t) : Src.Path.t Lazy.t =
      lazy Src.Path.(in_thread 0 @@ Thread.in_stms @@ path)

    let insert_live : Src.Path.With_meta.t Lazy.t =
      flag [] Src.Path.(thread_0_stms @@ Stms.insert pos_0_last_atom)

    let insert_start : Src.Path.With_meta.t Lazy.t =
      flag [] Src.Path.(thread_0_stms @@ Stms.insert 0)

    let insert_end : Src.Path.With_meta.t Lazy.t =
      flag []
        (Lazy.bind body_stms ~f:(fun stms ->
             Src.Path.(thread_0_stms @@ Stms.insert (List.length stms)) ) )

    let known_true_if (path : Src.Path.If.t) : Src.Path.t Lazy.t =
      Src.Path.(
        thread_0_stms @@ Stms.in_stm pos_0_true_if @@ Stm.in_if @@ path)

    let known_false_if (path : Src.Path.If.t) : Src.Path.t Lazy.t =
      Src.Path.(
        thread_0_stms @@ Stms.in_stm pos_0_false_if @@ Stm.in_if @@ path)

    let dead_else (path : Src.Path.Stms.t) : Src.Path.t Lazy.t =
      Src.Path.(known_true_if @@ If.in_branch false @@ path)

    let insert_dead : Src.Path.With_meta.t Lazy.t =
      flag [In_dead_code] Src.Path.(dead_else @@ Stms.insert 0)

    let once_loop (path : Src.Path.Flow.t) : Src.Path.t Lazy.t =
      Src.Path.(
        thread_0_stms
        @@ Stms.in_stm pos_0_once_do_while
        @@ Stm.in_flow @@ path)

    let insert_once_loop_end : Src.Path.With_meta.t Lazy.t =
      flag [In_loop] Src.Path.(once_loop @@ Flow.in_body @@ Stms.insert 1)

    let multi_loop (path : Src.Path.Flow.t) : Src.Path.t Lazy.t =
      Src.Path.(
        thread_0_stms @@ Stms.in_stm pos_0_multi_for @@ Stm.in_flow @@ path)

    let insert_multi_loop_end : Src.Path.With_meta.t Lazy.t =
      flag
        [In_execute_multi; In_loop]
        Src.Path.(multi_loop @@ Flow.in_body @@ Stms.insert 1)

    let dead_loop (path : Src.Path.Flow.t) : Src.Path.t Lazy.t =
      Src.Path.(
        thread_0_stms @@ Stms.in_stm pos_0_dead_while @@ Stm.in_flow @@ path)

    let insert_dead_loop : Src.Path.With_meta.t Lazy.t =
      flag [In_dead_code; In_loop]
        Src.Path.(dead_loop @@ Flow.in_body @@ Stms.insert 0)

    let in_stm : Src.Path.t Lazy.t = Src.Path.(thread_0_stms @@ Stms.stm 2)

    let in_stm_flagged : Src.Path.With_meta.t Lazy.t = flag [] in_stm

    let surround_atomic : Src.Path.With_meta.t Lazy.t =
      flag []
        Src.Path.(
          thread_0_stms @@ Stms.between pos_0_first_atom pos_0_last_atom)

    let surround_label_direct : Src.Path.With_meta.t Lazy.t =
      flag []
        Src.Path.(known_true_if @@ If.in_branch true @@ Stms.on_range 0 2)

    let surround_label_indirect : Src.Path.With_meta.t Lazy.t =
      flag []
        Src.Path.(thread_0_stms @@ Stms.between pos_0_true_if pos_0_false_if)

    let surround_dead : Src.Path.With_meta.t Lazy.t =
      flag [In_dead_code]
        Src.Path.(known_false_if @@ If.in_branch true @@ Stms.on_range 0 1)

    let surround_txsafe : Src.Path.With_meta.t Lazy.t =
      flag [] Src.Path.(thread_0_stms @@ Stms.singleton pos_0_nop)
  end
end

let%test_module "has_statements checks" =
  ( module struct
    let test (matching : Fir.Statement_class.t list) : unit =
      C4f_utils.Io.print_bool
        (Src.Subject.Test.has_statements ~matching
           (Lazy.force Test_data.test) )

    let%expect_test "has atomic blocks" =
      test [Flow (Some (Lock (Some Atomic)))] ;
      [%expect {| false |}]

    let%expect_test "has atomics" =
      test [Fir.Statement_class.atomic ()] ;
      [%expect {| true |}]
  end )

let%test_module "has_statements_not_matching" =
  ( module struct
    let test (one_of : Fir.Statement_class.t list) : unit =
      C4f_utils.Io.print_bool
        (Src.Subject.Test.has_statements_not_matching ~one_of
           (Lazy.force Test_data.test) )

    let%expect_test "has things that are not atomic blocks" =
      test [Flow (Some (Lock (Some Atomic)))] ;
      [%expect {| true |}]

    let%expect_test "has non-atomics" =
      test [Fir.Statement_class.atomic ()] ;
      [%expect {| true |}]

    let%expect_test "has things that are not a primitive, or not an if, or \
                     not a flow" =
      test [If; Flow None; Prim None] ;
      [%expect {| true |}]
  end )

let%test_module "list_to_litmus" =
  ( module struct
    type r = Fir.Litmus.Lang.Program.t list [@@deriving sexp_of]

    let env = Lazy.force C4f_fir_test.Env.test_env

    let vars : Src.Var.Map.t =
      env
      |> C4f_utils.My_map.map_with_keys
           (module C4f_common.Litmus_id)
           ~f:(fun ~key ~data ->
             let lit = C4f_common.Litmus_id.global key in
             let ty = Accessor.get Fir.Env.Record.type_of data in
             (lit, Src.Var.Record.make_existing Global ty) )
      |> Or_error.ok_exn |> C4f_common.Scoped_map.of_litmus_id_map

    let test programs =
      let res = Src.Subject.Thread.list_to_litmus programs ~vars in
      Fmt.(pr "@[<v>%a@]@." (list ~sep:sp C4f_litmus_c.Reify.pp_func) res)

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
            atomic_int r0 = 4004;
            int r1 = 8008;
            atomic_store_explicit(x, 42, memory_order_seq_cst);
            ;
            atomic_store_explicit(y, foo, memory_order_relaxed);
            if (foo == y)
            { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
            if (false)
            {
                atomic_store_explicit(y,
                                      atomic_load_explicit(x, memory_order_seq_cst),
                                      memory_order_seq_cst);
            }
            do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
            5);
            for (r1 = 0; r1 <= 2; r1++)
            { atomic_store_explicit(x, 99, memory_order_seq_cst); }
            while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
        }
        void
        P1(atomic_int *bar, bool barbaz, int *blep, int foo, atomic_bool *foobaz,
           atomic_int x, atomic_int y, atomic_bool z)
        { loop: ; if (true) {  } else { goto loop; } } |}]

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
            atomic_int r0 = 4004;
            int r1 = 8008;
            atomic_store_explicit(x, 42, memory_order_seq_cst);
            ;
            atomic_store_explicit(y, foo, memory_order_relaxed);
            if (foo == y)
            { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
            if (false)
            {
                atomic_store_explicit(y,
                                      atomic_load_explicit(x, memory_order_seq_cst),
                                      memory_order_seq_cst);
            }
            do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
            5);
            for (r1 = 0; r1 <= 2; r1++)
            { atomic_store_explicit(x, 99, memory_order_seq_cst); }
            while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
        }
        void
        P1(atomic_int *bar, bool barbaz, int *blep, int foo, atomic_bool *foobaz,
           atomic_int x, atomic_int y, atomic_bool z)
        { loop: ; if (true) {  } else { goto loop; } } |}]
  end )
