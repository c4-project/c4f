(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let dst_in_env env x =
  Map.mem env Fir.(x.@(Assign.dst @> Lvalue.variable_of))

let print_sample =
  Utils.My_quickcheck.print_sample
    ~printer:
      Fmt.(
        pr "@[%a@]@."
          (using
             (Accessor.construct
                (Fir.Statement.prim' @> Fir.Prim_statement.assign))
             Act_litmus_c.Reify_stm.pp))

let%expect_test "Int: samples" =
  let env = Lazy.force Fir_test.Env.test_env in
  print_sample
    ( module struct
      include Fir.Assign

      include Src.Assign.Int
                (struct
                  let env = env
                end)
                (struct
                  let env = env
                end)
    end ) ;
  [%expect
    {|
    foo--;
    foo++;
    foo = 0 & 4;
    foo = *blep ^ *blep;
    foo = 0 & -914050481 ^
    (*blep - *blep | 40877287 | atomic_load_explicit(bar, memory_order_relaxed) |
     (atomic_fetch_xor_explicit(&x, 0, memory_order_consume) |
      atomic_fetch_add_explicit(bar, 0, memory_order_seq_cst)));
    foo = atomic_load_explicit(bar, memory_order_consume) -
    atomic_load_explicit(bar, memory_order_consume) & *blep ^
    atomic_load_explicit(&x, memory_order_acquire);
    (*blep)--;
    (*blep)++;
    *blep = 18140;
    *blep = atomic_fetch_or_explicit(&x, 0 & 27, memory_order_seq_cst);
    *blep = 0 & foo ^ 0 & foo; |}]

let%test_unit "Int: generated destination variables in environment" =
  let env = Lazy.force Fir_test.Env.test_env in
  let module Env = struct
    let env = env
  end in
  let module Chk = Src.Assign.Int (Env) (Env) in
  Q.Test.run_exn
    (module Chk)
    ~f:([%test_pred: Fir.Assign.t] ~here:[[%here]] (dst_in_env env))

let%expect_test "Bool: samples" =
  let env = Lazy.force Fir_test.Env.test_env in
  print_sample
    ( module struct
      include Fir.Assign

      include Src.Assign.Bool
                (struct
                  let env = env
                end)
                (struct
                  let env = env
                end)
    end ) ;
  [%expect
    {|
    barbaz = false;
    barbaz = true;
    barbaz = barbaz;
    barbaz = atomic_load_explicit(&y, memory_order_acquire) - 53 ==
    (atomic_load_explicit(&y, memory_order_seq_cst) & 27 -
     atomic_fetch_add_explicit(&x,
                               atomic_load_explicit(bar, memory_order_seq_cst) -
                               atomic_load_explicit(bar, memory_order_seq_cst),
                               memory_order_consume));
    barbaz = (279118692 | atomic_load_explicit(&x, memory_order_seq_cst)) ==
    atomic_fetch_or_explicit(&x,
                             atomic_fetch_xor_explicit(&x, 51 - 51,
                                                       memory_order_acquire)
                             -
                             atomic_fetch_xor_explicit(&x, 51 - 51,
                                                       memory_order_acquire),
                             memory_order_seq_cst)
    -
    atomic_fetch_or_explicit(&x,
                             atomic_fetch_xor_explicit(&x, 51 - 51,
                                                       memory_order_acquire)
                             -
                             atomic_fetch_xor_explicit(&x, 51 - 51,
                                                       memory_order_acquire),
                             memory_order_seq_cst);
    barbaz = 0 != atomic_load_explicit(&x, memory_order_consume);
    barbaz = foo != (atomic_load_explicit(bar, memory_order_acquire) & 0);
    barbaz = (*blep & 0) >= atomic_load_explicit(&x, memory_order_relaxed);
    barbaz = 0 <= (0 ^ (atomic_load_explicit(&x, memory_order_consume) ^ 27)) &&
    barbaz;
    barbaz = (barbaz || atomic_load_explicit(bar, memory_order_consume) >= 0) &&
    !barbaz;
    barbaz =
    ((0 >= 0 && 0 == 0 || barbaz && false) &&
     (95 ^
      atomic_fetch_sub_explicit(bar,
                                atomic_load_explicit(bar, memory_order_relaxed) &
                                0, memory_order_consume))
     >= (10703535 & 0) || !!barbaz)
    && barbaz;
    barbaz =
    (!barbaz || (atomic_fetch_or_explicit(bar, 0, memory_order_acq_rel) & 0) <=
     (*blep & 437078))
    && atomic_load_explicit(&y, memory_order_consume) -
    atomic_load_explicit(&y, memory_order_consume) <=
    atomic_fetch_sub_explicit(bar, 95 -
                              atomic_load_explicit(bar, memory_order_consume),
                              memory_order_relaxed);
    barbaz = barbaz || barbaz &&
    ((53 ^ atomic_load_explicit(&y, memory_order_acquire)) &
     (atomic_load_explicit(bar, memory_order_relaxed) -
      atomic_load_explicit(bar, memory_order_relaxed) | *blep ^ *blep)
     & atomic_fetch_add_explicit(&y, 7998 - 7998, memory_order_acquire) ^ *blep -
     *blep)
    < 0;
    barbaz = !barbaz; |}]

let%test_unit "Bool: generated destination variables in environment" =
  let env = Lazy.force Fir_test.Env.test_env in
  let module Env = struct
    let env = env
  end in
  let module Chk = Src.Assign.Bool (Env) (Env) in
  Q.Test.run_exn
    (module Chk)
    ~f:([%test_pred: Fir.Assign.t] ~here:[[%here]] (dst_in_env env))
