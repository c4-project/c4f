(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck
module Src = Act_fuzz

let%test_module "Make" =
  ( module struct
    let%test_module "Payload" =
      ( module struct
        let%test_unit "constant is the right type" =
          Test.run_exn
            ( module struct
              type t = Src.Var_actions.Make_payload.t [@@deriving sexp]

              let quickcheck_generator =
                Src.Var_actions.Make_payload.generator
                  (Lazy.force Var.Test_data.test_map)
                  ~gen_scope:
                    (Base_quickcheck.Generator.return
                       Act_common.Scope.Global)

              let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
            end )
            ~f:
              (fun Src.Var_actions.Make_payload.{basic_type; initial_value; _}
                   ->
              let (_ : Act_c_mini.Type.t) =
                Or_error.ok_exn
                  Act_c_mini.(
                    Type.check_modulo_atomicity (Type.make basic_type)
                      (Constant.type_of initial_value))
              in
              ())
      end )

    let%test_module "Example runs" =
      ( module struct
        let test_action (payload : Src.Var_actions.Make_payload.t) :
            Src.Subject.Test.t Src.State.Monad.t =
          Src.Var_actions.Make.run
            (Lazy.force Subject.Test_data.test)
            ~payload

        let test (scope : Act_common.Scope.t) : unit =
          let pld =
            Src.Var_actions.Make_payload.
              { basic_type= Act_c_mini.Type.Basic.int ()
              ; initial_value= Act_c_mini.Constant.int 42
              ; var=
                  Act_common.Litmus_id.make ~scope
                    ~id:(Act_common.C_id.of_string "foo") }
          in
          let action = test_action pld in
          Action.Test_utils.run_and_dump_test action
            ~initial_state:(Lazy.force Subject.Test_data.state)

        let%expect_test "local" =
          test Act_common.Scope.(Local 1) ;
          [%expect
            {|
          void
          P0(atomic_int *x, atomic_int *y)
          {
              atomic_int r0 = 4004;
              atomic_store_explicit(x, 42, memory_order_seq_cst);
              ;
              atomic_store_explicit(y, foo, memory_order_relaxed);
              if (foo == y)
              { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
              if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
              do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
              5);
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
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(y, foo, memory_order_relaxed);
          if (foo == y)
          { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
          if (false) { atomic_store_explicit(y, 95, memory_order_seq_cst); }
          do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
          5);
      }

      void
      P1(int *foo, atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]
      end )
  end )
