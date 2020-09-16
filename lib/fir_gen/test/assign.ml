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
    foo = 0;
    foo = foo;
    foo =
    atomic_fetch_xor_explicit(bar,
                              atomic_fetch_add_explicit(bar, 0,
                                                        memory_order_acq_rel)
                              & 0, memory_order_seq_cst)
    -
    atomic_fetch_xor_explicit(bar,
                              atomic_fetch_add_explicit(bar, 0,
                                                        memory_order_acq_rel)
                              & 0, memory_order_seq_cst);
    foo =
    (95 ^
     atomic_fetch_add_explicit(bar,
                               atomic_load_explicit(&y, memory_order_acquire) -
                               53, memory_order_release))
    & *blep;
    foo = atomic_fetch_sub_explicit(bar, 0, memory_order_release) -
    atomic_fetch_sub_explicit(bar, 0, memory_order_release) &
    atomic_fetch_sub_explicit(&x, 0 & 27, memory_order_seq_cst) ^ 0 & 27;
    (*blep)--;
    (*blep)++;
    *blep = foo;
    *blep = (53 ^ atomic_load_explicit(&y, memory_order_acquire)) &
    (atomic_load_explicit(bar, memory_order_acquire) -
     atomic_load_explicit(bar, memory_order_acquire) |
     (foo | atomic_load_explicit(bar, memory_order_consume)))
    & 0;
    *blep = 0 & -914050481 ^
    (0 - 0 | atomic_fetch_xor_explicit(bar, 0, memory_order_acquire) & 0 | 0 |
     atomic_load_explicit(&x, memory_order_relaxed)); |}]

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
    barbaz = (0 & 0) == atomic_fetch_xor_explicit(&x, 0, memory_order_seq_cst);
    barbaz =
    ((*blep |
      (atomic_load_explicit(&y, memory_order_acquire) |
       (atomic_load_explicit(bar, memory_order_acquire) | foo ^ 4 ^
        atomic_load_explicit(&y, memory_order_seq_cst) - 53 |
        (atomic_fetch_sub_explicit(&x, 0 &
                                   atomic_fetch_sub_explicit(&x,
                                                             atomic_fetch_xor_explicit
                                                             (&x, 95 & 0,
                                                              memory_order_relaxed)
                                                             - 27,
                                                             memory_order_acquire),
                                   memory_order_relaxed)
         & 0 |
         atomic_fetch_xor_explicit(bar,
                                   atomic_fetch_xor_explicit(bar,
                                                             atomic_fetch_add_explicit
                                                             (bar, 0,
                                                              memory_order_release)
                                                             & 0,
                                                             memory_order_relaxed)
                                   ^ 95, memory_order_acquire)))))
     ^ (0 ^ 0))
    ==
    (atomic_fetch_sub_explicit(bar, 0, memory_order_release) &
     (atomic_fetch_sub_explicit(&y, 0 &
                                atomic_load_explicit(bar, memory_order_relaxed),
                                memory_order_release)
      & 0)
     & foo);
    barbaz = 0 != atomic_load_explicit(&x, memory_order_consume);
    barbaz = (4 & 0) >
    (foo |
     atomic_fetch_or_explicit(bar,
                              atomic_fetch_sub_explicit(&y, 0,
                                                        memory_order_seq_cst)
                              -
                              atomic_fetch_sub_explicit(&y, 0,
                                                        memory_order_seq_cst),
                              memory_order_relaxed)
     & atomic_fetch_xor_explicit(&x, 0, memory_order_acq_rel));
    barbaz = (0 & atomic_load_explicit(&y, memory_order_relaxed)) >=
    atomic_load_explicit(&x, memory_order_seq_cst);
    barbaz = (0 & -3937) < 0;
    barbaz =
    (95 ^
     atomic_fetch_add_explicit(bar,
                               atomic_load_explicit(&y, memory_order_acquire) -
                               53, memory_order_release))
    < *blep;
    barbaz = !atomic_load_explicit(foobaz, memory_order_consume) &&
    atomic_fetch_add_explicit(&y, 0, memory_order_release) >=
    (atomic_load_explicit(&y, memory_order_consume) &
     (atomic_fetch_xor_explicit(&y, 0, memory_order_release) -
      atomic_fetch_xor_explicit(&y, 0, memory_order_release) & -2054658))
    &&
    !!(atomic_fetch_xor_explicit(&y,
                                 atomic_fetch_xor_explicit(bar, 0,
                                                           memory_order_seq_cst)
                                 -
                                 atomic_fetch_xor_explicit(bar, 0,
                                                           memory_order_seq_cst),
                                 memory_order_seq_cst)
       >= atomic_load_explicit(bar, memory_order_relaxed) &&
       (atomic_load_explicit(&y, memory_order_consume) <=
        (atomic_load_explicit(&y, memory_order_relaxed) - 53 & foo) ||
        (!(((0 & atomic_load_explicit(&y, memory_order_consume) | (0 | 0)) !=
            (atomic_load_explicit(&x, memory_order_consume) | 0 & foo) || barbaz)
           &&
           !((0 ^ 0) < (atomic_load_explicit(&x, memory_order_seq_cst) & 0) ||
             (atomic_fetch_or_explicit(&y, 0, memory_order_seq_cst) & 0) > foo))
         || atomic_load_explicit(foobaz, memory_order_consume) ||
         (atomic_load_explicit(&y, memory_order_relaxed) ^ 53) <= (4 ^ foo)))
       &&
       (0 &
        atomic_fetch_or_explicit(bar, 0 &
                                 atomic_fetch_or_explicit(&x,
                                                          atomic_load_explicit
                                                          (&y,
                                                           memory_order_acquire)
                                                          & 0,
                                                          memory_order_release),
                                 memory_order_seq_cst))
       < (0 & foo) || barbaz || !atomic_load_explicit(&z, memory_order_seq_cst));
    barbaz = (barbaz || atomic_load_explicit(bar, memory_order_consume) >= 0) &&
    !atomic_load_explicit(foobaz, memory_order_seq_cst);
    barbaz =
    !(-914050481 !=
      ((0 - 0 | atomic_fetch_xor_explicit(bar, 0, memory_order_acquire) & 0 | 0 |
        atomic_load_explicit(&x, memory_order_relaxed))
       ^ (atomic_load_explicit(&y, memory_order_relaxed) ^ 53)))
    &&
    (atomic_load_explicit(&z, memory_order_relaxed) ||
     atomic_fetch_or_explicit(&x, 0 &
                              atomic_fetch_sub_explicit(&x, 0,
                                                        memory_order_release),
                              memory_order_acquire)
     >
     ((atomic_load_explicit(bar, memory_order_acquire) -
       atomic_load_explicit(bar, memory_order_acquire) |
       (foo | atomic_load_explicit(bar, memory_order_consume)) | 0)
      & 0));
    barbaz = barbaz ||
    (((atomic_load_explicit(bar, memory_order_relaxed) | foo - foo) & foo | foo)
     !=
     (atomic_fetch_xor_explicit(bar, 0, memory_order_relaxed) - 95 & 0 ^ 0 &
      atomic_fetch_sub_explicit(&x, 0, memory_order_acq_rel) |
      atomic_load_explicit(&y, memory_order_relaxed))
     || atomic_load_explicit(foobaz, memory_order_consume));
    barbaz = atomic_load_explicit(foobaz, memory_order_relaxed) || 0 ==
    123004066;
    barbaz = !barbaz;
    barbaz =
    !(atomic_fetch_sub_explicit(&x,
                                atomic_fetch_sub_explicit(&y, 0,
                                                          memory_order_release)
                                ^
                                atomic_fetch_sub_explicit(&y, 0,
                                                          memory_order_release),
                                memory_order_release)
      != *blep &&
      (0 & 27 ^
       atomic_fetch_sub_explicit(&x, 53 ^
                                 atomic_load_explicit(&y, memory_order_seq_cst),
                                 memory_order_relaxed)
       & 0 & 0)
      != (0 & *blep)); |}]

let%test_unit "Bool: generated destination variables in environment" =
  let env = Lazy.force Fir_test.Env.test_env in
  let module Env = struct
    let env = env
  end in
  let module Chk = Src.Assign.Bool (Env) (Env) in
  Q.Test.run_exn
    (module Chk)
    ~f:([%test_pred: Fir.Assign.t] ~here:[[%here]] (dst_in_env env))
