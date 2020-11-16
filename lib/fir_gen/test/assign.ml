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
    foo = 0;
    foo = 0 & (atomic_load_explicit(&x, memory_order_relaxed) | *blep & *blep) &
    0;
    foo = 95 - atomic_load_explicit(bar, memory_order_consume) &
    atomic_fetch_or_explicit(&y, atomic_load_explicit(bar, memory_order_consume)
                             - 95, memory_order_acq_rel)
    ^ foo - 4 & (0 | atomic_fetch_add_explicit(&x, 0, memory_order_release));
    (*blep)--;
    (*blep)++;
    *blep = atomic_fetch_add_explicit(bar, 0, memory_order_relaxed);
    *blep =
    atomic_fetch_xor_explicit(&x,
                              atomic_fetch_sub_explicit(&y, 0 &
                                                        atomic_load_explicit
                                                        (&y,
                                                         memory_order_consume),
                                                        memory_order_relaxed)
                              & 0, memory_order_acquire);
    *blep = atomic_load_explicit(&y, memory_order_consume);
    *blep = 0 & atomic_fetch_sub_explicit(&y, 4 ^ foo, memory_order_relaxed);
    *blep = atomic_load_explicit(bar, memory_order_acquire) - 95 | 0 ^
    atomic_fetch_xor_explicit(&x,
                              atomic_fetch_sub_explicit(bar, 99 & 0,
                                                        memory_order_acquire)
                              & 0, memory_order_relaxed)
    | atomic_load_explicit(&y, memory_order_seq_cst) - 53 |
    (foo ^ atomic_fetch_or_explicit(bar, 0 & 53, memory_order_acquire) |
     (*blep | 2147483647)); |}]

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
    barbaz = atomic_load_explicit(foobaz, memory_order_consume);
    barbaz =
    (atomic_fetch_sub_explicit(&y, 0, memory_order_consume) |
     atomic_load_explicit(&x, memory_order_consume) &
     (foo - 4 ^ (atomic_load_explicit(bar, memory_order_seq_cst) ^ 95)))
    != atomic_load_explicit(bar, memory_order_relaxed) - 95;
    barbaz =
    atomic_fetch_sub_explicit(&x, 27 ^
                              atomic_fetch_xor_explicit(&x, *blep - 99,
                                                        memory_order_seq_cst),
                              memory_order_acquire)
    > *blep;
    barbaz =
    (atomic_load_explicit(&x, memory_order_consume) &
     atomic_load_explicit(&x, memory_order_seq_cst))
    >= *blep;
    barbaz =
    (atomic_fetch_or_explicit(&x,
                              atomic_fetch_or_explicit(bar, 0 &
                                                       atomic_load_explicit
                                                       (&x, memory_order_acquire),
                                                       memory_order_relaxed)
                              -
                              atomic_fetch_or_explicit(bar, 0 &
                                                       atomic_load_explicit
                                                       (&x, memory_order_acquire),
                                                       memory_order_relaxed)
                              & 0, memory_order_seq_cst)
     &
     (95 ^ atomic_load_explicit(bar, memory_order_seq_cst) ^
      (95 ^ atomic_load_explicit(bar, memory_order_seq_cst)))
     & atomic_load_explicit(&y, memory_order_acquire) &
     atomic_load_explicit(&x, memory_order_relaxed))
    <= atomic_fetch_sub_explicit(bar, 99 & 0, memory_order_relaxed);
    barbaz = barbaz &&
    !(atomic_fetch_sub_explicit(&x, 0, memory_order_relaxed) <= (foo & -2548));
    barbaz = atomic_load_explicit(foobaz, memory_order_consume) &&
    (barbaz || atomic_load_explicit(&x, memory_order_seq_cst) <=
     (atomic_load_explicit(bar, memory_order_seq_cst) ^
      ((atomic_load_explicit(bar, memory_order_relaxed) | 4 - foo) &
       atomic_fetch_or_explicit(&x,
                                atomic_load_explicit(&x, memory_order_acquire) -
                                27, memory_order_seq_cst)
       | (1 ^ 0) - (1 ^ 0) ^
       (foo - foo | atomic_load_explicit(&y, memory_order_relaxed) ^
        atomic_load_explicit(&x, memory_order_consume)))
      | 0));
    barbaz =
    (atomic_fetch_sub_explicit(&y, 0 &
                               atomic_load_explicit(&y, memory_order_consume),
                               memory_order_relaxed)
     & 0)
    <=
    ((95 ^ atomic_load_explicit(bar, memory_order_relaxed)) &
     atomic_load_explicit(&x, memory_order_relaxed))
    && *blep > *blep;
    barbaz =
    ((true && atomic_load_explicit(&z, memory_order_consume) ||
      (atomic_load_explicit(&y, memory_order_relaxed) -
       atomic_load_explicit(&y, memory_order_relaxed) ==
       (atomic_fetch_sub_explicit(bar, 0, memory_order_relaxed) &
        atomic_load_explicit(&x, memory_order_consume))
       || atomic_fetch_xor_explicit(&y, 0, memory_order_relaxed) ==
       (atomic_fetch_xor_explicit(bar, 0, memory_order_acquire) | 0)))
     && false || barbaz)
    &&
    (true ||
     (*blep >
      (99 - *blep | 0 & 0 &
       (atomic_fetch_add_explicit(&y, 0, memory_order_acq_rel) & 0))
      || atomic_load_explicit(&z, memory_order_acquire)));
    barbaz =
    (barbaz && atomic_fetch_sub_explicit(bar, 0, memory_order_acq_rel) <=
     atomic_load_explicit(&y, memory_order_acquire) ||
     (atomic_load_explicit(bar, memory_order_acquire) |
      atomic_fetch_or_explicit(bar, 0 &
                               (atomic_fetch_xor_explicit(bar,
                                                          atomic_fetch_add_explicit
                                                          (bar, foo - foo,
                                                           memory_order_acquire)
                                                          & 0,
                                                          memory_order_acq_rel)
                                ^
                                (atomic_load_explicit(&y, memory_order_seq_cst) ^
                                 (atomic_fetch_add_explicit(&x, *blep & 0,
                                                            memory_order_relaxed)
                                  ^
                                  atomic_fetch_add_explicit(bar, 0 & 0,
                                                            memory_order_acq_rel)
                                  | *blep))),
                               memory_order_relaxed)
      & 0 ^ atomic_load_explicit(&x, memory_order_acquire))
     <
     (0 &
      (0 & (*blep & 0 & atomic_load_explicit(bar, memory_order_relaxed) - 95)))
     ||
     (atomic_fetch_xor_explicit(&y, 27 -
                                atomic_fetch_sub_explicit(&x,
                                                          atomic_load_explicit
                                                          (&y,
                                                           memory_order_relaxed)
                                                          & 0,
                                                          memory_order_consume)
                                ^ 27 -
                                atomic_fetch_sub_explicit(&x,
                                                          atomic_load_explicit
                                                          (&y,
                                                           memory_order_relaxed)
                                                          & 0,
                                                          memory_order_consume),
                                memory_order_seq_cst)
      | foo)
     <= atomic_load_explicit(&y, memory_order_acquire))
    &&
    ((barbaz && atomic_load_explicit(foobaz, memory_order_seq_cst) || true) &&
     !barbaz);
    barbaz = true || false;
    barbaz = 0 > atomic_load_explicit(bar, memory_order_consume) ||
    atomic_load_explicit(&y, memory_order_acquire) >
    atomic_fetch_add_explicit(&y, 0, memory_order_release);
    barbaz = false ||
    !((atomic_fetch_sub_explicit(&y, 99 - *blep, memory_order_acq_rel) &
       atomic_fetch_add_explicit(bar, (foo | *blep | *blep ^ (0 | *blep)) -
                                 (foo | *blep | *blep ^ (0 | *blep)),
                                 memory_order_release))
      > 0)
    || barbaz;
    barbaz =
    !(foo !=
      atomic_fetch_add_explicit(&x,
                                atomic_fetch_sub_explicit(bar, 0 & (foo & 0),
                                                          memory_order_release)
                                & 0, memory_order_release)); |}]

let%test_unit "Bool: generated destination variables in environment" =
  let env = Lazy.force Fir_test.Env.test_env in
  let module Env = struct
    let env = env
  end in
  let module Chk = Src.Assign.Bool (Env) (Env) in
  Q.Test.run_exn
    (module Chk)
    ~f:([%test_pred: Fir.Assign.t] ~here:[[%here]] (dst_in_env env))

let%expect_test "any is defined on test env" =
  let env = Lazy.force Fir_test.Env.test_env in
  let asn = Src.Assign.any ~src:env ~dst:env in
  Stdio.printf "%b" (Option.is_some asn) ;
  [%expect {| true |}]

let%expect_test "any is NOT defined on empty env" =
  let env = Fir.Env.of_typing (Map.empty (module Act_common.C_id)) in
  let asn = Src.Assign.any ~src:env ~dst:env in
  Stdio.printf "%b" (Option.is_some asn) ;
  [%expect {| false |}]

let%expect_test "any: samples with valid env" =
  let env = Lazy.force Fir_test.Env.test_env in
  print_sample
    ( module struct
      include Fir.Assign

      let quickcheck_generator =
        Option.value_exn (Src.Assign.any ~src:env ~dst:env)

      let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
    end ) ;
  [%expect
    {|
      barbaz = false;
      barbaz = barbaz;
      barbaz = (atomic_load_explicit(bar, memory_order_consume) ^ 0 | foo) ==
      atomic_fetch_sub_explicit(&y, atomic_load_explicit(bar, memory_order_consume)
                                - 95, memory_order_consume);
      barbaz = (atomic_load_explicit(&x, memory_order_seq_cst) & 0) !=
      (0 & (atomic_load_explicit(bar, memory_order_acquire) - 95 | foo - foo) ^
       (atomic_load_explicit(&y, memory_order_relaxed) ^
        atomic_load_explicit(&x, memory_order_consume))
       | 0 |
       (atomic_load_explicit(&x, memory_order_consume) &
        atomic_load_explicit(&x, memory_order_seq_cst) | *blep)
       ^
       atomic_fetch_sub_explicit(&x, 27 ^
                                 atomic_load_explicit(&x, memory_order_seq_cst),
                                 memory_order_acq_rel));
      barbaz = foo <
      atomic_fetch_add_explicit(bar,
                                atomic_fetch_sub_explicit(bar, foo &
                                                          (atomic_fetch_sub_explicit
                                                           (bar,
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire)
                                                            -
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire),
                                                            memory_order_release)
                                                           ^
                                                           atomic_fetch_sub_explicit
                                                           (bar,
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire)
                                                            -
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire),
                                                            memory_order_release))
                                                          ^ foo &
                                                          (atomic_fetch_sub_explicit
                                                           (bar,
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire)
                                                            -
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire),
                                                            memory_order_release)
                                                           ^
                                                           atomic_fetch_sub_explicit
                                                           (bar,
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire)
                                                            -
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire),
                                                            memory_order_release)),
                                                          memory_order_release)
                                -
                                atomic_fetch_sub_explicit(bar, foo &
                                                          (atomic_fetch_sub_explicit
                                                           (bar,
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire)
                                                            -
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire),
                                                            memory_order_release)
                                                           ^
                                                           atomic_fetch_sub_explicit
                                                           (bar,
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire)
                                                            -
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire),
                                                            memory_order_release))
                                                          ^ foo &
                                                          (atomic_fetch_sub_explicit
                                                           (bar,
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire)
                                                            -
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire),
                                                            memory_order_release)
                                                           ^
                                                           atomic_fetch_sub_explicit
                                                           (bar,
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire)
                                                            -
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire),
                                                            memory_order_release)),
                                                          memory_order_release),
                                memory_order_release);
      barbaz =
      (atomic_fetch_sub_explicit(&y, 0 &
                                 atomic_load_explicit(&y, memory_order_consume),
                                 memory_order_relaxed)
       & 0)
      <=
      ((95 ^ atomic_load_explicit(bar, memory_order_relaxed)) &
       atomic_load_explicit(&x, memory_order_relaxed))
      && *blep > *blep;
      barbaz = !atomic_load_explicit(&z, memory_order_seq_cst) || barbaz;
      foo++;
      foo = 0;
      foo = atomic_fetch_add_explicit(&y, 0, memory_order_seq_cst) ^ 0;
      (*blep)--;
      (*blep)++;
      *blep =
      atomic_fetch_sub_explicit(&y, 95 ^
                                atomic_load_explicit(bar, memory_order_seq_cst),
                                memory_order_seq_cst);
      *blep = atomic_load_explicit(&x, memory_order_consume);
      *blep = atomic_load_explicit(&y, memory_order_seq_cst) &
      (53 ^
       atomic_fetch_xor_explicit(&y,
                                 atomic_load_explicit(bar, memory_order_acquire) &
                                 0, memory_order_release));
      *blep =
      (atomic_load_explicit(bar, memory_order_acquire) ^ 95 -
       atomic_load_explicit(bar, memory_order_relaxed) |
       atomic_load_explicit(&y, memory_order_seq_cst) - 53)
      &
      (foo ^ atomic_fetch_or_explicit(bar, 0 & 53, memory_order_acquire) |
       (*blep | 2147483647)); |}]

let%test_unit "any: generated destination variables in environment" =
  let env = Lazy.force Fir_test.Env.test_env in
  Q.Test.run_exn
    ( module struct
      type t = Fir.Assign.t [@@deriving sexp]

      let quickcheck_generator =
        Option.value_exn (Src.Assign.any ~src:env ~dst:env)

      let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
    end )
    ~f:([%test_pred: Fir.Assign.t] ~here:[[%here]] (dst_in_env env))
