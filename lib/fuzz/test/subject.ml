(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Src = Act_fuzz
end

module Test_data = struct
  let init : Act_fir.Constant.t Act_common.C_named.Alist.t Lazy.t =
    lazy
      [ (Act_common.C_id.of_string "x", Act_fir.Constant.int 27)
      ; (Act_common.C_id.of_string "y", Act_fir.Constant.int 53) ]

  let body_decls : Act_fir.Initialiser.t Act_common.C_named.Alist.t Lazy.t =
    lazy
      [ ( Act_common.C_id.of_string "r0"
        , Act_fir.Initialiser.make
            ~ty:(Act_fir.Type.int ~is_atomic:true ())
            ~value:(Act_fir.Constant.int 4004) ) ]

  let globals : Act_fir.Type.t Act_common.C_named.Alist.t Lazy.t =
    lazy
      Act_fir.
        [ ( Act_common.C_id.of_string "x"
          , Type.(int ~is_pointer:true ~is_atomic:true ()) )
        ; ( Act_common.C_id.of_string "y"
          , Type.(int ~is_pointer:true ~is_atomic:true ()) ) ]

  let thread0_vars :
      (Act_common.Litmus_id.t, Act_fuzz.Var.Record.t) List.Assoc.t Lazy.t =
    Lazy.map body_decls
      ~f:
        (List.map ~f:(fun (id, v) ->
             let scope = Act_common.Scope.Local 0 in
             ( Act_common.Litmus_id.make ~scope ~id
             , Act_fuzz.Var.Record.make_generated
                 ~initial_value:(Accessor.get Act_fir.Initialiser.value v)
                 scope
                 (Accessor.get Act_fir.Initialiser.ty v) )))

  let state : Act_fuzz.State.t Lazy.t =
    (* TODO(@MattWindsor91): labels? *)
    Lazy.Let_syntax.(
      let%map globals_alist = globals in
      let global_vars =
        globals_alist
        |> List.map ~f:(fun (id, ty) ->
               ( Act_common.Litmus_id.global id
               , Act_fuzz.Var.Record.make_existing Global ty ))
      in
      let vars =
        global_vars @ Lazy.force thread0_vars
        |> Map.of_alist_exn (module Act_common.Litmus_id)
        |> Act_common.Scoped_map.of_litmus_id_map
      in
      Act_fuzz.State.make ~vars ())

  let mk_store (a : Act_fir.Atomic_store.t) : Src.Subject.Statement.t =
    Act_fir.(
      Statement.prim Src.Metadata.generated
      @@ Prim_statement.atomic_store @@ a)

  let mk_always_true_if (cond : Act_fir.Expression.t)
      (t : Src.Subject.Statement.t list) (f : Src.Subject.Statement.t list) :
      Src.Subject.Statement.t =
    Act_fir.(
      Statement.if_stm
        (Act_fir.If.make ~cond
           ~t_branch:(Src.Subject.Block.make_generated ~statements:t ())
           ~f_branch:(Src.Subject.Block.make_dead_code ~statements:f ())))

  let sample_known_true_if : Src.Subject.Statement.t Lazy.t =
    lazy
      Act_fir.(
        mk_always_true_if
          Expression.(
            Infix.(of_variable_str_exn "foo" == of_variable_str_exn "y"))
          [ mk_store
              (Atomic_store.make ~src:(Expression.int_lit 56)
                 ~dst:(Address.of_variable_str_exn "x")
                 ~mo:Mem_order.Seq_cst)
          ; Act_fir.Statement.prim Src.Metadata.generated
              Act_fir.(
                Prim_statement.label
                  (Act_common.C_id.of_string "kappa_kappa")) ]
          [])

  let sample_known_false_if : Src.Subject.Statement.t Lazy.t =
    lazy
      Act_fir.(
        Statement.if_stm
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
                                   ~mo:Mem_order.Seq_cst))
                           ~dst:(Address.of_variable_str_exn "y")
                           ~mo:Mem_order.Seq_cst) ]
                  ())
             ~f_branch:(Src.Subject.Block.make_generated ())))

  let sample_once_do_while : Src.Subject.Statement.t Lazy.t =
    lazy
      Act_fir.(
        Statement.flow
          (Flow_block.while_loop
             ~cond:Expression.(Infix.(int_lit 4 == int_lit 5))
             ~body:
               (Src.Subject.Block.make_dead_code
                  ~statements:
                    [ mk_store
                        (Atomic_store.make ~src:(Expression.int_lit 44)
                           ~dst:(Address.of_variable_str_exn "x")
                           ~mo:Mem_order.Seq_cst) ]
                  ())
             ~kind:Do_while))

  let body_stms : Src.Subject.Statement.t list Lazy.t =
    Lazy.Let_syntax.(
      let%map sample_known_true_if = sample_known_true_if
      and sample_known_false_if = sample_known_false_if
      and sample_once_do_while = sample_once_do_while in
      Act_fir.(
        Statement.
          [ mk_store
              (Atomic_store.make ~src:(Expression.int_lit 42)
                 ~dst:(Address.of_variable_str_exn "x")
                 ~mo:Mem_order.Seq_cst)
          ; prim Src.Metadata.generated Prim_statement.nop
          ; mk_store
              (Atomic_store.make
                 ~src:(Expression.of_variable_str_exn "foo")
                 ~dst:(Address.of_variable_str_exn "y")
                 ~mo:Mem_order.Relaxed)
          ; sample_known_true_if
          ; sample_known_false_if
          ; sample_once_do_while ]))

  let pos_0_first_atom = 0

  let pos_0_nop = 1

  let pos_0_last_atom = 2

  let pos_0_true_if = 3

  let pos_0_false_if = 4

  let pos_0_do_while = 5

  let thread0 : Src.Subject.Thread.t Lazy.t =
    Lazy.Let_syntax.(
      let%map stms = body_stms and decls = body_decls in
      Src.Subject.Thread.make ~stms ~decls ())

  let thread1_stms : Src.Subject.Statement.t list Lazy.t =
    lazy
      Act_fir.(
        Statement.
          [ prim Src.Metadata.generated
              (Prim_statement.label (Act_common.C_id.of_string "loop"))
          ; mk_always_true_if Act_fir.Expression.truth []
              [ prim Src.Metadata.generated
                  (Prim_statement.goto (Act_common.C_id.of_string "loop")) ]
          ])

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
      let header = Act_litmus.Header.make ~name:"example" ~init () in
      Act_litmus.Test.Raw.make ~header ~threads)

  module Path = struct
    (* These will need manually synchronising with the statements above. *)

    let flag (fs : Src.Path_flag.t list) (p : Src.Path.t Lazy.t) :
        Src.Path.t Src.Path_flag.Flagged.t Lazy.t =
      Lazy.map p ~f:(fun path ->
          let flags = Set.of_list (module Src.Path_flag) fs in
          Src.Path_flag.Flagged.make ~path ~flags)

    let thread_0_stms (path : Src.Path.Stms.t) : Src.Path.t Lazy.t =
      lazy Src.Path.(in_thread 0 @@ Thread.in_stms @@ path)

    let insert_live : Src.Path.Flagged.t Lazy.t =
      flag [] Src.Path.(thread_0_stms @@ Stms.insert pos_0_last_atom)

    let insert_start : Src.Path.Flagged.t Lazy.t =
      flag [] Src.Path.(thread_0_stms @@ Stms.insert 0)

    let insert_end : Src.Path.Flagged.t Lazy.t =
      flag []
        (Lazy.bind body_stms ~f:(fun stms ->
             Src.Path.(thread_0_stms @@ Stms.insert (List.length stms))))

    let known_true_if (path : Src.Path.If.t) : Src.Path.t Lazy.t =
      Src.Path.(
        thread_0_stms @@ Stms.in_stm pos_0_true_if @@ Stm.in_if @@ path)

    let known_false_if (path : Src.Path.If.t) : Src.Path.t Lazy.t =
      Src.Path.(
        thread_0_stms @@ Stms.in_stm pos_0_false_if @@ Stm.in_if @@ path)

    let dead_else (path : Src.Path.Stms.t) : Src.Path.t Lazy.t =
      Src.Path.(known_true_if @@ If.in_branch false @@ path)

    let insert_dead : Src.Path.Flagged.t Lazy.t =
      flag [In_dead_code] Src.Path.(dead_else @@ Stms.insert 0)

    let dead_loop (path : Src.Path.Flow.t) : Src.Path.t Lazy.t =
      Src.Path.(
        thread_0_stms @@ Stms.in_stm pos_0_do_while @@ Stm.in_flow @@ path)

    let insert_dead_loop : Src.Path.Flagged.t Lazy.t =
      flag [In_dead_code; In_loop]
        Src.Path.(dead_loop @@ Flow.in_body @@ Stms.insert 0)

    let in_stm : Src.Path.t Lazy.t = Src.Path.(thread_0_stms @@ Stms.stm 2)

    let in_stm_flagged : Src.Path.Flagged.t Lazy.t = flag [] in_stm

    let surround_atomic : Src.Path.Flagged.t Lazy.t =
      flag []
        Src.Path.(
          thread_0_stms @@ Stms.between pos_0_first_atom pos_0_last_atom)

    let surround_label_direct : Src.Path.Flagged.t Lazy.t =
      flag []
        Src.Path.(known_true_if @@ If.in_branch true @@ Stms.on_range 0 2)

    let surround_label_indirect : Src.Path.Flagged.t Lazy.t =
      flag []
        Src.Path.(thread_0_stms @@ Stms.between pos_0_true_if pos_0_false_if)

    let surround_dead : Src.Path.Flagged.t Lazy.t =
      flag [In_dead_code]
        Src.Path.(known_false_if @@ If.in_branch true @@ Stms.on_range 0 1)

    let surround_txsafe : Src.Path.Flagged.t Lazy.t =
      flag [] Src.Path.(thread_0_stms @@ Stms.singleton pos_0_nop)
  end
end

let%test_module "has_statements checks" =
  ( module struct
    let test (matching : Act_fir.Statement_class.t list) : unit =
      Act_utils.Io.print_bool
        (Src.Subject.Test.has_statements ~matching
           (Lazy.force Test_data.test))

    let%expect_test "has atomic blocks" =
      test [Flow (Some (Lock (Some Atomic)))] ;
      [%expect {| false |}]

    let%expect_test "has atomics" =
      test [Act_fir.Statement_class.atomic ()] ;
      [%expect {| true |}]
  end )

let%test_module "has_statements_not_matching" =
  ( module struct
    let test (one_of : Act_fir.Statement_class.t list) : unit =
      Act_utils.Io.print_bool
        (Src.Subject.Test.has_statements_not_matching ~one_of
           (Lazy.force Test_data.test))

    let%expect_test "has things that are not atomic blocks" =
      test [Flow (Some (Lock (Some Atomic)))] ;
      [%expect {| true |}]

    let%expect_test "has non-atomics" =
      test [Act_fir.Statement_class.atomic ()] ;
      [%expect {| true |}]

    let%expect_test "has things that are not a primitive, or not an if, or \
                     not a flow" =
      test [If; Flow None; Prim None] ;
      [%expect {| true |}]
  end )

let%test_module "list_to_litmus" =
  ( module struct
    type r = Act_fir.Litmus.Lang.Program.t list [@@deriving sexp_of]

    let env = Lazy.force Act_fir_test.Env.test_env

    let vars : Src.Var.Map.t =
      env
      |> Act_utils.My_map.map_with_keys
           (module Act_common.Litmus_id)
           ~f:(fun ~key ~data ->
             let lit = Act_common.Litmus_id.global key in
             let ty = Accessor.get Act_fir.Env.Record.type_of data in
             (lit, Src.Var.Record.make_existing Global ty))
      |> Or_error.ok_exn |> Act_common.Scoped_map.of_litmus_id_map

    let test programs =
      let res = Src.Subject.Thread.list_to_litmus programs ~vars in
      Fmt.(pr "@[<v>%a@]@." (list ~sep:sp Act_litmus_c.Reify.pp_func) res)

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
        }
        void
        P1(atomic_int *bar, bool barbaz, int *blep, int foo, atomic_bool *foobaz,
           atomic_int x, atomic_int y, atomic_bool z)
        { loop: ; if (true) {  } else { goto loop; } } |}]
  end )
