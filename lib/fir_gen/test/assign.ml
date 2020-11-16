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
    foo = 0 & (53 & 0);
    (*blep)--;
    (*blep)++;
    *blep = 126030 | 95 - atomic_load_explicit(bar, memory_order_seq_cst) |
    (27 - atomic_load_explicit(&x, memory_order_relaxed) | -1048576);
    *blep = (foo ^ foo) & *blep ^ foo; |}]

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
    barbaz = true;
    barbaz = barbaz;
    barbaz = 27 - atomic_load_explicit(&x, memory_order_relaxed) == -1048576;
    barbaz = 0 !=
    (0 & (53 & 0) ^
     atomic_fetch_xor_explicit(&x, 126030 & 0, memory_order_acquire));
    barbaz = (foo & 0) > (0 & *blep);
    barbaz = 0 >= *blep;
    barbaz = (0 & atomic_load_explicit(bar, memory_order_seq_cst)) >=
    (atomic_fetch_xor_explicit(&x, 0, memory_order_consume) | 0 |
     atomic_load_explicit(&x, memory_order_acquire) ^ 0);
    barbaz = *blep - *blep <= foo;
    barbaz = 0 != foo && false;
    barbaz =
    (!((atomic_load_explicit(&x, memory_order_consume) ^ 0) ==
       (atomic_load_explicit(&x, memory_order_acquire) |
        (atomic_load_explicit(bar, memory_order_relaxed) |
         atomic_load_explicit(&x, memory_order_acquire))))
     || 0 <=
     (atomic_load_explicit(&x, memory_order_relaxed) &
      atomic_load_explicit(bar, memory_order_relaxed) &
      atomic_fetch_sub_explicit(&x, foo - foo, memory_order_relaxed)))
    &&
    (0 & -4 ^
     (atomic_load_explicit(&y, memory_order_acquire) ^
      (foo - foo | (27 ^ atomic_fetch_sub_explicit(&x, 0, memory_order_consume))
       & (foo | atomic_load_explicit(bar, memory_order_relaxed) & 0 & 0) |
       atomic_load_explicit(&x, memory_order_relaxed))))
    != 0;
    barbaz = false ||
    (true ||
     atomic_fetch_sub_explicit(&y, -524288 ^ -524288, memory_order_acquire) >
     (((atomic_load_explicit(bar, memory_order_acquire) |
        atomic_load_explicit(bar, memory_order_seq_cst))
       ^
       atomic_fetch_xor_explicit(&x, 2147483647 - 2147483647,
                                 memory_order_seq_cst))
      &
      (atomic_fetch_add_explicit(&x, 0 &
                                 atomic_fetch_add_explicit(&x,
                                                           atomic_load_explicit
                                                           (bar,
                                                            memory_order_acquire)
                                                           - 95,
                                                           memory_order_seq_cst),
                                 memory_order_seq_cst)
       ^ (atomic_fetch_or_explicit(bar, 0, memory_order_acq_rel) ^ 95))));
    barbaz = atomic_load_explicit(foobaz, memory_order_relaxed) ||
    (barbaz ||
     (*blep <=
      ((atomic_fetch_sub_explicit(bar, 0, memory_order_acq_rel) &
        (foo ^ foo | 0 &
         atomic_fetch_xor_explicit(bar, 0 & 27, memory_order_relaxed) &
         atomic_load_explicit(&y, memory_order_acquire))
        | 0 & 95)
       & 0)
      ||
      (atomic_load_explicit(bar, memory_order_seq_cst) != 0 ||
       atomic_load_explicit(&z, memory_order_consume) || barbaz && *blep >=
       *blep))
     || barbaz &&
     (0 > foo ||
      (atomic_fetch_add_explicit(bar,
                                 atomic_fetch_xor_explicit(&x, *blep - 99,
                                                           memory_order_release)
                                 -
                                 atomic_fetch_xor_explicit(&x, *blep - 99,
                                                           memory_order_release),
                                 memory_order_acquire)
       &
       (0 & atomic_load_explicit(&y, memory_order_seq_cst) ^
        (atomic_fetch_xor_explicit(&y, 0, memory_order_acq_rel) ^
         atomic_fetch_xor_explicit(&y, 0, memory_order_acq_rel))
        ^
        (0 &
         atomic_fetch_or_explicit(&x, 95 ^
                                  atomic_load_explicit(bar, memory_order_seq_cst),
                                  memory_order_consume)
         | atomic_load_explicit(&y, memory_order_relaxed))
        & 1919995593))
      >
      (53 & 0 |
       (0 & atomic_fetch_xor_explicit(&x, 0 & foo, memory_order_release) |
        atomic_load_explicit(&y, memory_order_seq_cst) ^ 53 ^
        (0 |
         ((0 ^
           (atomic_fetch_add_explicit(bar, 0, memory_order_seq_cst) ^ 0 & 4 &
            atomic_fetch_sub_explicit(bar,
                                      atomic_load_explicit(&x,
                                                           memory_order_acquire)
                                      ^ 27, memory_order_release)
            & ((0 | *blep) ^ (foo ^ 4)) & (0 & foo))
           ^ 0)
          & 0 | 0 & atomic_load_explicit(&y, memory_order_consume)))))));
    barbaz = atomic_load_explicit(foobaz, memory_order_consume) || true;
    barbaz = 0 <= foo || 0 >
    atomic_fetch_or_explicit(&y, 0, memory_order_acquire);
    barbaz = atomic_load_explicit(&x, memory_order_relaxed) >=
    (foo & atomic_fetch_add_explicit(bar, 0, memory_order_acq_rel)) ||
    (foo ^ 2789) != (0 & atomic_load_explicit(&y, memory_order_relaxed)) ||
    !!atomic_load_explicit(&z, memory_order_acquire) &&
    ((false || false) &&
     (atomic_load_explicit(foobaz, memory_order_consume) &&
      (barbaz ||
       atomic_fetch_xor_explicit(bar,
                                 atomic_fetch_sub_explicit(&y, foo ^ foo,
                                                           memory_order_seq_cst)
                                 & 0, memory_order_seq_cst)
       < -18166770)
      || (foo & 0) >=
      (atomic_fetch_xor_explicit(bar, 0, memory_order_relaxed) - 95 & 0 ^ *blep &
       0 |
       (atomic_load_explicit(&x, memory_order_acquire) |
        atomic_load_explicit(&y, memory_order_relaxed))
       |
       atomic_fetch_or_explicit(bar,
                                atomic_load_explicit(&y, memory_order_acquire) -
                                atomic_load_explicit(&y, memory_order_acquire),
                                memory_order_relaxed)
       ^
       (atomic_fetch_or_explicit(&y, 0, memory_order_acq_rel) |
        atomic_load_explicit(&y, memory_order_consume) -
        atomic_load_explicit(&y, memory_order_consume) &
        atomic_load_explicit(bar, memory_order_seq_cst))
       ^ atomic_fetch_xor_explicit(&y, 0 & 27, memory_order_relaxed))));
    barbaz =
    !((barbaz || foo < atomic_fetch_xor_explicit(&x, 0, memory_order_relaxed)) &&
      atomic_fetch_sub_explicit(&x, 0 &
                                atomic_fetch_or_explicit(bar, foo ^ 4,
                                                         memory_order_consume),
                                memory_order_consume)
      < (0 ^ 1 | atomic_load_explicit(&y, memory_order_acquire)))
    || atomic_load_explicit(&z, memory_order_acquire);
    barbaz = !true;
    barbaz =
    !(atomic_load_explicit(&x, memory_order_seq_cst) <=
      atomic_fetch_xor_explicit(bar,
                                atomic_fetch_sub_explicit(&x, 0,
                                                          memory_order_release)
                                -
                                atomic_fetch_sub_explicit(&x, 0,
                                                          memory_order_release),
                                memory_order_consume));
    barbaz =
    !(((0 & atomic_fetch_sub_explicit(&x, 0 & *blep, memory_order_seq_cst) |
        (foo |
         (*blep - 99 & (0 ^ *blep) ^
          atomic_fetch_sub_explicit(&y,
                                    atomic_load_explicit(bar,
                                                         memory_order_acquire)
                                    ^ 95, memory_order_seq_cst)
          | 0 & atomic_load_explicit(&y, memory_order_consume) &
          atomic_load_explicit(&x, memory_order_acquire))))
       & atomic_fetch_or_explicit(&x, 0 & 99, memory_order_relaxed) &
       (0 & atomic_fetch_xor_explicit(&x, *blep & 0, memory_order_seq_cst)))
      == (0 & *blep) &&
      (((atomic_fetch_or_explicit(&x, 0, memory_order_seq_cst) | foo ^ 0) &
        *blep)
       !=
       ((atomic_fetch_or_explicit(&y, *blep ^ *blep, memory_order_acq_rel) ^ 0) &
        atomic_load_explicit(bar, memory_order_relaxed))
       || atomic_load_explicit(&z, memory_order_consume) ||
       (0 & foo & atomic_load_explicit(&x, memory_order_consume)) >
       atomic_load_explicit(bar, memory_order_acquire))); |}]

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
      barbaz = true;
      barbaz = foo >= 0;
      barbaz = (0 & *blep) >= foo;
      barbaz = (atomic_load_explicit(&y, memory_order_relaxed) & 0) <= (0 & *blep);
      barbaz = barbaz &&
      !(atomic_fetch_sub_explicit(&x, 0 &
                                  atomic_fetch_or_explicit(bar, foo ^ 4,
                                                           memory_order_consume),
                                  memory_order_consume)
        < (0 ^ 1 | atomic_load_explicit(&y, memory_order_acquire)));
      barbaz =
      (true && atomic_load_explicit(&z, memory_order_consume) ||
       (0 <= (atomic_load_explicit(&x, memory_order_seq_cst) ^ 0) ||
        (0 > 0 || barbaz)))
      && atomic_load_explicit(&y, memory_order_seq_cst) !=
      (atomic_fetch_sub_explicit(&x,
                                 atomic_load_explicit(bar, memory_order_relaxed) -
                                 atomic_load_explicit(bar, memory_order_relaxed),
                                 memory_order_acquire)
       ^ 0 & foo | foo ^ 0 & 53);
      barbaz = atomic_load_explicit(&x, memory_order_relaxed) >=
      (foo & atomic_fetch_add_explicit(bar, 0, memory_order_acq_rel)) ||
      (foo ^ 2789) != (0 & atomic_load_explicit(&y, memory_order_relaxed)) ||
      !!atomic_load_explicit(&z, memory_order_acquire) &&
      ((false || false) &&
       (atomic_load_explicit(foobaz, memory_order_consume) &&
        (barbaz ||
         atomic_fetch_xor_explicit(bar,
                                   atomic_fetch_sub_explicit(&y, foo ^ foo,
                                                             memory_order_seq_cst)
                                   & 0, memory_order_seq_cst)
         < -18166770)
        || (foo & 0) >=
        (atomic_fetch_xor_explicit(bar, 0, memory_order_relaxed) - 95 & 0 ^ *blep &
         0 |
         (atomic_load_explicit(&x, memory_order_acquire) |
          atomic_load_explicit(&y, memory_order_relaxed))
         |
         atomic_fetch_or_explicit(bar,
                                  atomic_load_explicit(&y, memory_order_acquire) -
                                  atomic_load_explicit(&y, memory_order_acquire),
                                  memory_order_relaxed)
         ^
         (atomic_fetch_or_explicit(&y, 0, memory_order_acq_rel) |
          atomic_load_explicit(&y, memory_order_consume) -
          atomic_load_explicit(&y, memory_order_consume) &
          atomic_load_explicit(bar, memory_order_seq_cst))
         ^ atomic_fetch_xor_explicit(&y, 0 & 27, memory_order_relaxed))));
      barbaz =
      !(atomic_load_explicit(&x, memory_order_seq_cst) <=
        atomic_fetch_xor_explicit(bar,
                                  atomic_fetch_sub_explicit(&x, 0,
                                                            memory_order_release)
                                  -
                                  atomic_fetch_sub_explicit(&x, 0,
                                                            memory_order_release),
                                  memory_order_consume));
      barbaz =
      !((atomic_fetch_xor_explicit(&x, 0, memory_order_consume) | 0 |
         atomic_load_explicit(&x, memory_order_acquire) ^ 0)
        <= atomic_load_explicit(&y, memory_order_relaxed) && barbaz);
      foo--;
      foo++;
      foo = 0;
      (*blep)--;
      *blep = -112015996 ^ atomic_fetch_or_explicit(&x, 0, memory_order_seq_cst); |}]

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
