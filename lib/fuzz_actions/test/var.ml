(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck

open struct
  module Ac = Act_common
  module Fir = Act_fir
  module Src = Act_fuzz_actions
  module F = Act_fuzz
  module FT = Act_fuzz_test
  module Qx = Act_utils.My_quickcheck
end

let%test_module "Make" =
  ( module struct
    let%test_module "Payload" =
      ( module struct
        let printer
            ({basic_type; initial_value; var} : Src.Var.Make_payload.t) :
            unit =
          Fmt.pr "@[%s@ %a@ =@ %a;@]@."
            (Fir.Type.Basic.to_string basic_type)
            Ac.Litmus_id.pp var Act_litmus_c.Reify_prim.pp_constant
            initial_value

        let make_gen (scope : Ac.Scope.t) :
            (module Qx.S_sample with type t = Src.Var.Make_payload.t) =
          ( module struct
            include Src.Var.Make_payload

            let quickcheck_generator =
              Src.Var.Make_payload.generator
                (Lazy.force FT.Var.Test_data.test_map)
                ~gen_scope:(Base_quickcheck.Generator.return scope)
          end )

        let make_gen' (scope : Ac.Scope.t) :
            (module Test.S with type t = Src.Var.Make_payload.t) =
          (* TODO(@MattWindsor91): SURELY there's a better way of doing this *)
          ( module struct
            include (val make_gen scope)
          end )

        let print_sample (scope : Ac.Scope.t) : unit =
          (* Expressions are quite big, so we tone down the generation
             parameters a bit. *)
          Qx.print_sample ~test_count:5 ~printer (make_gen scope)

        let%expect_test "global: samples" =
          print_sample Global ;
          [%expect
            {|
            int opulent_yurt_1 = -24;
            int xylophone = 22551631;
            atomic_int robust_game_1 = -153;
            atomic_int xylophone_0 = -1;
            atomic_int zebra = 11676625; |}]

        let%expect_test "thread 0: samples" =
          print_sample (Local 0) ;
          [%expect
            {|
            bool 0:jocular_easel = true;
            atomic_bool 0:kelp_0 = true;
            atomic_bool 0:opulent_heap = true;
            atomic_int 0:strong_zebra = -1;
            atomic_int 0:xylophone_0 = 33417; |}]

        let test_type (scope : Ac.Scope.t) : unit =
          Test.run_exn (make_gen' scope)
            ~f:(fun Src.Var.Make_payload.{basic_type; initial_value; _} ->
              let (_ : Act_fir.Type.t) =
                Or_error.ok_exn
                  Act_fir.(
                    Type.check_modulo_atomicity (Type.make basic_type)
                      (Constant.type_of initial_value))
              in
              ())

        let%test_unit "global: constant is the right type" = test_type Global

        let%test_unit "thread 0: constant is the right type" =
          test_type (Local 0)
      end )

    let%test_module "Example runs" =
      ( module struct
        let test_action (payload : Src.Var.Make_payload.t) :
            F.Subject.Test.t F.State.Monad.t =
          Src.Var.Make.run (Lazy.force FT.Subject.Test_data.test) ~payload

        let test (scope : Act_common.Scope.t) : unit =
          let pld =
            Src.Var.Make_payload.
              { basic_type= Act_fir.Type.Basic.int ()
              ; initial_value= Act_fir.Constant.int 42
              ; var=
                  Act_common.Litmus_id.make ~scope
                    ~id:(Act_common.C_id.of_string "foo") }
          in
          let action = test_action pld in
          FT.Action.Test_utils.run_and_dump_test action
            ~initial_state:(Lazy.force FT.Subject.Test_data.state)

        let%expect_test "local" =
          test Act_common.Scope.(Local 1) ;
          [%expect
            {|
          void
          P0(atomic_int *x, atomic_int *y)
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
          P1(atomic_int *x, atomic_int *y)
          { int foo = 42; loop: ; if (true) {  } else { goto loop; } } |}]

        let%expect_test "global" =
          test Act_common.Scope.Global ;
          [%expect
            {|
      void
      P0(int *foo, atomic_int *x, atomic_int *y)
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
      P1(int *foo, atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]
      end )
  end )

let%test_module "Volatile" =
  ( module struct
    let%test_module "Example runs" =
      ( module struct
        let test_action (payload : Act_common.Litmus_id.t) :
            F.Subject.Test.t F.State.Monad.t =
          Src.Var.Volatile.run
            (Lazy.force FT.Subject.Test_data.test)
            ~payload

        let test (tid : int) (name : string) : unit =
          let pld =
            Act_common.Litmus_id.local tid (Act_common.C_id.of_string name)
          in
          let action = test_action pld in
          FT.Action.Test_utils.run_and_dump_test action
            ~initial_state:(Lazy.force FT.Subject.Test_data.state)

        let%expect_test "r0" =
          test 0 "r0" ;
          [%expect
            {|
            void
            P0(atomic_int *x, atomic_int *y)
            {
                atomic_int volatile r0 = 4004;
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
            P1(atomic_int *x, atomic_int *y)
            { loop: ; if (true) {  } else { goto loop; } } |}]
      end )
  end )
