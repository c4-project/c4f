(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

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
  let store (name : string) : Act_c_mini.Atomic_store.t Lazy.t =
    lazy
      Act_c_mini.(
        Atomic_store.make
          ~src:
            (Expression.atomic_load
               (Atomic_load.make
                  ~src:(Address.of_variable_str_exn "gen2")
                  ~mo:Mem_order.Seq_cst))
          ~dst:(Address.of_variable_str_exn name)
          ~mo:Mem_order.Seq_cst)

  let fadd : Act_c_mini.Expression.t Lazy.t =
    lazy
      Act_c_mini.(
        Expression.(
          atomic_fetch
            (Atomic_fetch.make
               ~obj:(Address.of_variable_str_exn "gen1")
               ~arg:(Expression.int_lit 0) ~mo:Seq_cst ~op:Add)))

  let store_fa : Act_c_mini.Atomic_store.t Lazy.t =
    lazy
      Act_c_mini.(
        Atomic_store.make
          ~src:
            Act_c_mini.(
              Expression.(
                atomic_fetch
                  (Atomic_fetch.make
                     ~obj:(Address.of_variable_str_exn "gen1")
                     ~arg:(sub (Lazy.force fadd) (Lazy.force fadd))
                     ~mo:Seq_cst ~op:Sub)))
          ~dst:(Address.of_variable_str_exn "gen1")
          ~mo:Seq_cst)

  let prepare_fuzzer_state () : unit Src.State.Monad.t =
    Src.State.Monad.(
      register_var
        Act_c_mini.Type.(int ~pointer:true ~atomic:true ())
        (Act_common.Litmus_id.of_string "gen1")
        ~initial_value:(Act_c_mini.Constant.int 1337)
      >>= fun () ->
      register_var
        Act_c_mini.Type.(int ~pointer:true ~atomic:true ())
        (Act_common.Litmus_id.of_string "gen2")
        ~initial_value:(Act_c_mini.Constant.int (-55)))
end

let run_and_dump_vars (test_action : Src.Subject.Test.t Src.State.Monad.t)
    ~(predicates : (Src.Var.Record.t -> bool) list)
    ~(initial_state : Src.State.t) : unit =
  let result =
    Or_error.(
      Src.State.Monad.(run' test_action initial_state)
      >>| fst
      >>| Src.State.vars_satisfying_all ~scope:(Local 0) ~predicates)
  in
  Fmt.(
    pr "@[%a@]@."
      (result ~error:Error.pp ~ok:(list ~sep:sp Act_common.C_id.pp)))
    result

let%test_module "store.make.int.normal" =
  ( module struct
    let path : Src.Path.Program.t Lazy.t = Subject.Test_data.Path.insert_live

    let random_state (store : Act_c_mini.Atomic_store.t Lazy.t) :
        Src.Store_actions.Int.Payload.t Lazy.t =
      Lazy.Let_syntax.(
        let%bind to_insert = store in
        let%map where = path in
        Src.Payload.Insertion.make ~to_insert ~where)

    let test_action (store : Act_c_mini.Atomic_store.t Lazy.t) :
        Src.Subject.Test.t Src.State.Monad.t =
      Src.State.Monad.(
        Test_data.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Store_actions.Int.run
          (Lazy.force Subject.Test_data.test)
          ~payload:(Lazy.force (random_state store)))

    let%test_module "store of load to global" =
      ( module struct
        let test_action : Src.Subject.Test.t Src.State.Monad.t =
          test_action (Test_data.store "gen1")

        let%expect_test "test int store: programs" =
          Action.Test_utils.run_and_dump_test test_action
            ~initial_state:(Lazy.force Subject.Test_data.state) ;
          [%expect
            {|
      void
      P0(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      {
          atomic_int r0 = 4004;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(gen1,
                                atomic_load_explicit(gen2, memory_order_seq_cst),
                                memory_order_seq_cst);
          atomic_store_explicit(y, foo, memory_order_relaxed);
          if (foo == y)
          { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
          if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
          do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
          5);
      }

      void
      P1(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

        let%expect_test "test int store: global variables" =
          run_and_dump_vars test_action
            ~initial_state:(Lazy.force Subject.Test_data.state)
            ~predicates:[Src.Var.Record.is_global] ;
          [%expect {| gen1 gen2 x y |}]

        let%expect_test "test int store: variables with known values" =
          run_and_dump_vars test_action
            ~initial_state:(Lazy.force Subject.Test_data.state)
            ~predicates:[Src.Var.Record.has_known_value] ;
          [%expect {| gen2 r0 |}]

        let%expect_test "test int store: variables with dependencies" =
          run_and_dump_vars test_action
            ~initial_state:(Lazy.force Subject.Test_data.state)
            ~predicates:[Src.Var.Record.has_dependencies] ;
          [%expect {| gen2 |}]
      end )

    let%test_module "store of load to local" =
      ( module struct
        let test_action : Src.Subject.Test.t Src.State.Monad.t =
          test_action (Test_data.store "r0")

        let%expect_test "test int store: programs" =
          Action.Test_utils.run_and_dump_test test_action
            ~initial_state:(Lazy.force Subject.Test_data.state) ;
          [%expect
            {|
      void
      P0(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      {
          atomic_int r0 = 4004;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(r0,
                                atomic_load_explicit(gen2, memory_order_seq_cst),
                                memory_order_seq_cst);
          atomic_store_explicit(y, foo, memory_order_relaxed);
          if (foo == y)
          { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
          if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
          do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
          5);
      }

      void
      P1(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

        let%expect_test "test int store: global variables" =
          run_and_dump_vars test_action
            ~initial_state:(Lazy.force Subject.Test_data.state)
            ~predicates:[Src.Var.Record.is_global] ;
          [%expect {| gen1 gen2 x y |}]

        let%expect_test "test int store: variables with known values" =
          run_and_dump_vars test_action
            ~initial_state:(Lazy.force Subject.Test_data.state)
            ~predicates:[Src.Var.Record.has_known_value] ;
          [%expect {| gen1 gen2 |}]

        let%expect_test "test int store: variables with dependencies" =
          run_and_dump_vars test_action
            ~initial_state:(Lazy.force Subject.Test_data.state)
            ~predicates:[Src.Var.Record.has_dependencies] ;
          [%expect {| gen2 |}]
      end )

    let%test_module "store of self-referential fetching" =
      ( module struct
        let test_action : Src.Subject.Test.t Src.State.Monad.t =
          test_action Test_data.store_fa

        let%expect_test "test int store: programs" =
          Action.Test_utils.run_and_dump_test test_action
            ~initial_state:(Lazy.force Subject.Test_data.state) ;
          [%expect
            {|
      void
      P0(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      {
          atomic_int r0 = 4004;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(gen1,
                                atomic_fetch_sub_explicit(gen1,
                                                          atomic_fetch_add_explicit
                                                          (gen1, 0,
                                                           memory_order_seq_cst)
                                                          -
                                                          atomic_fetch_add_explicit
                                                          (gen1, 0,
                                                           memory_order_seq_cst),
                                                          memory_order_seq_cst),
                                memory_order_seq_cst);
          atomic_store_explicit(y, foo, memory_order_relaxed);
          if (foo == y)
          { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
          if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
          do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
          5);
      }

      void
      P1(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

        let%expect_test "test int store: global variables" =
          run_and_dump_vars test_action
            ~initial_state:(Lazy.force Subject.Test_data.state)
            ~predicates:[Src.Var.Record.is_global] ;
          [%expect {| gen1 gen2 x y |}]

        let%expect_test "test int store: variables with known values" =
          run_and_dump_vars test_action
            ~initial_state:(Lazy.force Subject.Test_data.state)
            ~predicates:[Src.Var.Record.has_known_value] ;
          [%expect {| gen2 r0 |}]

        let%expect_test "test int store: variables with dependencies" =
          run_and_dump_vars test_action
            ~initial_state:(Lazy.force Subject.Test_data.state)
            ~predicates:[Src.Var.Record.has_dependencies] ;
          [%expect {| gen1 |}]
      end )
  end )

let%test_module "store.make.int.dead" =
  ( module struct
    let path : Src.Path.Program.t Lazy.t = Subject.Test_data.Path.insert_dead

    let random_state : Src.Store_actions.Int.Payload.t Lazy.t =
      Lazy.Let_syntax.(
        let%bind to_insert = Test_data.store "gen1" in
        let%map where = path in
        Src.Payload.Insertion.make ~to_insert ~where)

    let test_action : Src.Subject.Test.t Src.State.Monad.t =
      Src.State.Monad.(
        Test_data.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Store_actions.Int_dead.run
          (Lazy.force Subject.Test_data.test)
          ~payload:(Lazy.force random_state))

    let%expect_test "test int store: programs" =
      Action.Test_utils.run_and_dump_test test_action
        ~initial_state:(Lazy.force Subject.Test_data.state) ;
      [%expect
        {|
      void
      P0(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      {
          atomic_int r0 = 4004;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(y, foo, memory_order_relaxed);
          if (foo == y)
          { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
          else
          {
              atomic_store_explicit(gen1,
                                    atomic_load_explicit(gen2,
                                                         memory_order_seq_cst),
                                    memory_order_seq_cst);
          }
          if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
          do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
          5);
      }

      void
      P1(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

    let%expect_test "test int store: global variables" =
      run_and_dump_vars test_action
        ~initial_state:(Lazy.force Subject.Test_data.state)
        ~predicates:[Src.Var.Record.is_global] ;
      [%expect {| gen1 gen2 x y |}]

    let%expect_test "test int store: variables with known values" =
      run_and_dump_vars test_action
        ~initial_state:(Lazy.force Subject.Test_data.state)
        ~predicates:[Src.Var.Record.has_known_value] ;
      [%expect {| gen1 gen2 r0 |}]

    let%expect_test "test int store: variables with dependencies" =
      run_and_dump_vars test_action
        ~initial_state:(Lazy.force Subject.Test_data.state)
        ~predicates:[Src.Var.Record.has_dependencies] ;
      [%expect {| |}]
  end )

let%test_module "store.make.int.redundant" =
  ( module struct
    let path : Src.Path.Program.t Lazy.t = Subject.Test_data.Path.insert_live

    (* TODO(@MattWindsor91): this is, ironically, overly redundant. *)
    let redundant_store : Act_c_mini.Atomic_store.t Lazy.t =
      lazy
        Act_c_mini.(
          Atomic_store.make
            ~src:(Act_c_mini.Expression.int_lit 1337)
            ~dst:(Address.of_variable (Act_common.C_id.of_string "gen1"))
            ~mo:Mem_order.Seq_cst)

    let random_state : Src.Store_actions.Int.Payload.t Lazy.t =
      Lazy.Let_syntax.(
        let%bind to_insert = redundant_store in
        let%map where = path in
        Src.Payload.Insertion.make ~to_insert ~where)

    let test_action : Src.Subject.Test.t Src.State.Monad.t =
      Src.State.Monad.(
        Test_data.prepare_fuzzer_state ()
        >>= fun () ->
        Src.Store_actions.Int_redundant.run
          (Lazy.force Subject.Test_data.test)
          ~payload:(Lazy.force random_state))

    let%expect_test "test int store: programs" =
      Action.Test_utils.run_and_dump_test test_action
        ~initial_state:(Lazy.force Subject.Test_data.state) ;
      [%expect
        {|
      void
      P0(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      {
          atomic_int r0 = 4004;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(gen1, 1337, memory_order_seq_cst);
          atomic_store_explicit(y, foo, memory_order_relaxed);
          if (foo == y)
          { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
          if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
          do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
          5);
      }

      void
      P1(atomic_int *gen1, atomic_int *gen2, atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]

    let%expect_test "test int store: global variables" =
      run_and_dump_vars test_action
        ~initial_state:(Lazy.force Subject.Test_data.state)
        ~predicates:[Src.Var.Record.is_global] ;
      [%expect {| gen1 gen2 x y |}]

    let%expect_test "test int store: variables with known values" =
      run_and_dump_vars test_action
        ~initial_state:(Lazy.force Subject.Test_data.state)
        ~predicates:[Src.Var.Record.has_known_value] ;
      [%expect {| gen1 gen2 r0 |}]

    let%expect_test "test int store: variables with dependencies" =
      run_and_dump_vars test_action
        ~initial_state:(Lazy.force Subject.Test_data.state)
        ~predicates:[Src.Var.Record.has_dependencies] ;
      [%expect {| |}]
  end )
