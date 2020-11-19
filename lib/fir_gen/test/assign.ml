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
    foo = (*blep & *blep) - (*blep & *blep);
    foo = 95 - atomic_load_explicit(bar, memory_order_consume) &
    atomic_fetch_and_explicit(&x, false ?
                              atomic_fetch_or_explicit(&x, 0,
                                                       memory_order_acquire)
                              : -1, memory_order_acq_rel)
    ^
    atomic_fetch_add_explicit(&y, false ?
                              atomic_fetch_add_explicit(bar, 0,
                                                        memory_order_acq_rel)
                              : 0, memory_order_consume);
    (*blep)--;
    (*blep)++;
    *blep = atomic_load_explicit(&x, memory_order_relaxed);
    *blep = barbaz ? 0 > 0 ? true ? 0 : 0 : 0 & foo :
    atomic_load_explicit(bar, memory_order_consume);
    *blep = atomic_load_explicit(&z, memory_order_seq_cst) ? 0 + 0 : 0 | 0 |
    atomic_fetch_xor_explicit(bar, 0 ^ 0, memory_order_relaxed); |}]

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
    barbaz = atomic_load_explicit(foobaz, memory_order_acquire);
    barbaz = *blep ==
    (true ? atomic_load_explicit(bar, memory_order_acquire) :
     atomic_fetch_add_explicit(bar, 0, memory_order_consume));
    barbaz = (0 & atomic_load_explicit(bar, memory_order_seq_cst)) !=
    (0 != atomic_fetch_xor_explicit(bar, 0, memory_order_seq_cst) &&
     atomic_fetch_or_explicit(&y, 0, memory_order_seq_cst) <
     atomic_load_explicit(bar, memory_order_consume) ?
     atomic_fetch_add_explicit(&y, 0, memory_order_release) > foo ? foo : 2249 :
     atomic_load_explicit(&x, memory_order_consume) ^ foo);
    barbaz = (true ? 0 : 0) >
    (atomic_load_explicit(&y, memory_order_seq_cst) & *blep);
    barbaz =
    ((foo ^ atomic_load_explicit(&x, memory_order_consume)) >=
     atomic_fetch_add_explicit(&y,
                               atomic_fetch_and_explicit(&x, -1,
                                                         memory_order_release)
                               ^ 27, memory_order_relaxed)
     ? true ? 0 : atomic_load_explicit(&x, memory_order_relaxed) :
     atomic_fetch_and_explicit(&x, -1 + 0, memory_order_release))
    < 0;
    barbaz = barbaz && barbaz;
    barbaz = *blep <= (0 & 95 ^ foo) &&
    atomic_fetch_xor_explicit(bar,
                              atomic_fetch_xor_explicit(&x, 0 - 0 - (0 - 0),
                                                        memory_order_release)
                              - 27, memory_order_relaxed)
    >=
    ((barbaz || 0 > 0 ? atomic_load_explicit(&y, memory_order_consume) & *blep :
      *blep)
     ^
     (!barbaz ? *blep <= 0 ? *blep : 0 : barbaz ? 0 :
      atomic_fetch_and_explicit(&y, -1, memory_order_acquire)));
    barbaz =
    (((barbaz ? 0 : atomic_fetch_xor_explicit(bar, 0, memory_order_seq_cst)) ^
      ((0 | 0) ^ 0) |
      (atomic_load_explicit(&x, memory_order_consume) - 27 | 0 - 0 |
       (atomic_load_explicit(foobaz, memory_order_consume) ? 0 : 0) ^
       atomic_load_explicit(&y, memory_order_seq_cst) ^
       (*blep | atomic_fetch_sub_explicit(bar, 0, memory_order_relaxed)) &
       (0 <= -26143178 ? 0 : atomic_load_explicit(&x, memory_order_relaxed))))
     >=
     (atomic_fetch_and_explicit(&x, false ?
                                atomic_load_explicit(bar, memory_order_seq_cst) |
                                foo : -1, memory_order_release)
      |
      (atomic_load_explicit(bar, memory_order_relaxed) > 0 || true ? 0 |
       atomic_fetch_and_explicit(&x, -1, memory_order_consume) :
       atomic_fetch_sub_explicit(&x, 53 ^
                                 atomic_fetch_sub_explicit(&y, 0,
                                                           memory_order_release),
                                 memory_order_release)))
     ||
     (((*blep - 99 | 0 & 0) &
       (atomic_fetch_sub_explicit(&x, 0, memory_order_acquire) !=
        atomic_fetch_xor_explicit(&y, 0, memory_order_relaxed) ?
        atomic_load_explicit(&x, memory_order_relaxed) : 0))
      >= atomic_load_explicit(bar, memory_order_acquire) ||
      !(atomic_load_explicit(&x, memory_order_acquire) >
        (true ? atomic_fetch_add_explicit(bar, 0, memory_order_seq_cst) & 0 :
         true ? 0 : 0))))
    &&
    (barbaz ||
     (atomic_fetch_sub_explicit(&x,
                                atomic_load_explicit(&y, memory_order_consume) &
                                0, memory_order_consume)
      <=
      (false ? true ? 0 : 0 :
       atomic_fetch_sub_explicit(bar,
                                 atomic_fetch_xor_explicit(bar, 0,
                                                           memory_order_seq_cst)
                                 ^
                                 atomic_fetch_xor_explicit(bar, 0,
                                                           memory_order_seq_cst),
                                 memory_order_seq_cst))
      || (0 & (atomic_load_explicit(&y, memory_order_acquire) ^ (0 | 0) | *blep))
      !=
      (*blep |
       (false ? atomic_fetch_xor_explicit(bar, 0, memory_order_consume) : 0)))
     || false);
    barbaz =
    ((0 <= atomic_fetch_add_explicit(&y, 0, memory_order_acquire) || false) &&
     (atomic_fetch_add_explicit(&x, 0, memory_order_relaxed) <= 0 &&
      atomic_load_explicit(bar, memory_order_relaxed) == 0)
     || atomic_load_explicit(foobaz, memory_order_relaxed))
    &&
    (!barbaz || !(atomic_fetch_xor_explicit(&x, 0, memory_order_relaxed) > 0));
    barbaz =
    (atomic_fetch_xor_explicit(&y,
                               atomic_fetch_add_explicit(&x, 0,
                                                         memory_order_seq_cst)
                               ^
                               atomic_fetch_add_explicit(&x, 0,
                                                         memory_order_seq_cst),
                               memory_order_acquire)
     ^
     atomic_fetch_xor_explicit(&y,
                               atomic_fetch_add_explicit(&x, 0,
                                                         memory_order_seq_cst)
                               ^
                               atomic_fetch_add_explicit(&x, 0,
                                                         memory_order_seq_cst),
                               memory_order_acquire)
     ^
     (0 != 0 ?
      atomic_fetch_or_explicit(bar, 0 &
                               atomic_load_explicit(bar, memory_order_consume),
                               memory_order_seq_cst)
      : atomic_fetch_and_explicit(&y, -1, memory_order_release) ^
      atomic_load_explicit(bar, memory_order_seq_cst)))
    != *blep ||
    !(atomic_fetch_xor_explicit(bar,
                                atomic_fetch_sub_explicit(&y,
                                                          atomic_load_explicit
                                                          (&y,
                                                           memory_order_relaxed)
                                                          & 0,
                                                          memory_order_release)
                                & 0, memory_order_consume)
      <
      (false ? atomic_load_explicit(bar, memory_order_seq_cst) - 95 : true ? 0 :
       foo));
    barbaz = 0 > atomic_load_explicit(bar, memory_order_consume) ||
    atomic_load_explicit(&y, memory_order_acquire) !=
    atomic_fetch_or_explicit(&x, 0, memory_order_acquire);
    barbaz = *blep > *blep ||
    atomic_fetch_add_explicit(&y, false ? 0 + 0 : 0 | 0, memory_order_release) >
    (true ? 0 : atomic_load_explicit(&y, memory_order_relaxed) - 53) ||
    (false || !!atomic_load_explicit(&z, memory_order_acquire) &&
     ((false || false) &&
      (atomic_load_explicit(foobaz, memory_order_consume) &&
       (barbaz || atomic_load_explicit(&x, memory_order_seq_cst) <=
        ((atomic_load_explicit(bar, memory_order_seq_cst) >
          atomic_fetch_add_explicit(&y, 0, memory_order_seq_cst) ? 0 : -29)
         |
         atomic_fetch_or_explicit(&x,
                                  atomic_load_explicit(&x, memory_order_acquire)
                                  - 27, memory_order_seq_cst)))
       ||
       (false ||
        (atomic_load_explicit(foobaz, memory_order_relaxed) && (0 & 0 | 0 & 0) <=
         atomic_fetch_add_explicit(bar,
                                   atomic_load_explicit(&x, memory_order_consume)
                                   - 27, memory_order_acquire)
         ||
         (!((0 & atomic_load_explicit(&y, memory_order_acquire)) >=
            (atomic_load_explicit(&y, memory_order_consume) & foo))
          || (-738911604 | foo) <=
          (atomic_fetch_and_explicit(&x, -1, memory_order_acq_rel) < 0 ? 0 : 0)))))));
    barbaz =
    (0 &
     ((true > true ? atomic_load_explicit(&z, memory_order_relaxed) ? 0 :
       atomic_fetch_and_explicit(bar, -1, memory_order_acquire) : 0 | 0)
      &
      ((true ? 0 : 0) & 27 -
       atomic_fetch_sub_explicit(&x, 0 ^ 0, memory_order_release) &
       (0 |
        atomic_fetch_or_explicit(&y, 0 & 0 |
                                 atomic_fetch_or_explicit(bar, 0,
                                                          memory_order_seq_cst)
                                 & 0, memory_order_seq_cst))
       & (0 & (0 ^ 0)))))
    >= atomic_load_explicit(&y, memory_order_relaxed) ||
    ((barbaz ? foo ^ 27 & 0 :
      atomic_fetch_and_explicit(&y, -1 ^ 0, memory_order_consume))
     & atomic_load_explicit(&y, memory_order_seq_cst))
    >=
    (atomic_fetch_sub_explicit(&x, 0, memory_order_acquire) > 27 ?
     atomic_fetch_or_explicit(bar, false ?
                              atomic_fetch_xor_explicit(bar, 0,
                                                        memory_order_consume)
                              : 0, memory_order_seq_cst)
     : true ? 0 : atomic_fetch_or_explicit(bar, 0, memory_order_relaxed))
    ||
    (atomic_fetch_add_explicit(&x, barbaz || true ? 95 -
                               atomic_fetch_xor_explicit(bar, 0 & -1,
                                                         memory_order_acq_rel)
                               : false ?
                               atomic_fetch_or_explicit(&x, 0,
                                                        memory_order_seq_cst)
                               : foo, memory_order_consume)
     |
     (barbaz && !barbaz ? *blep ==
      atomic_fetch_xor_explicit(&x, 0, memory_order_acq_rel) ? 0 : 0 :
      atomic_load_explicit(foobaz, memory_order_seq_cst) ?
      atomic_load_explicit(bar, memory_order_seq_cst) : 0))
    ==
    (atomic_load_explicit(bar, memory_order_consume) |
     (false ?
      (atomic_load_explicit(&x, memory_order_relaxed) |
       atomic_load_explicit(&y, memory_order_consume))
      ^ *blep : (false ? 0 : atomic_load_explicit(bar, memory_order_consume)) &
      atomic_fetch_xor_explicit(&y, 0 &
                                atomic_fetch_sub_explicit(bar, 0,
                                                          memory_order_acquire),
                                memory_order_consume))
     & (*blep ^ 99) & (0 ^ atomic_load_explicit(&x, memory_order_consume))); |}]

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
      barbaz = true;
      barbaz = barbaz;
      barbaz =
      (foo ^
       (atomic_fetch_add_explicit(bar, 0, memory_order_consume) == 0 ?
        atomic_fetch_add_explicit(&y, 0, memory_order_acq_rel) : 0)
       & 2147483647)
      >= atomic_load_explicit(&y, memory_order_seq_cst) - 53;
      barbaz = 4095 <= atomic_load_explicit(&y, memory_order_consume);
      barbaz = 0 <
      ((atomic_load_explicit(&z, memory_order_seq_cst) ? 0 :
        atomic_fetch_and_explicit(bar, -1, memory_order_relaxed))
       ^
       atomic_fetch_add_explicit(bar,
                                 atomic_load_explicit(bar, memory_order_acquire) -
                                 95, memory_order_release));
      barbaz = atomic_load_explicit(foobaz, memory_order_consume) &&
      (barbaz || atomic_load_explicit(&x, memory_order_seq_cst) >
       ((0 <= atomic_fetch_sub_explicit(bar, 0, memory_order_release) || 0 >= 0 ? 4
         - foo : (0 | atomic_fetch_xor_explicit(bar, 0, memory_order_relaxed)) ^ 0
         & atomic_fetch_xor_explicit(bar, 0, memory_order_relaxed))
        |
        (foo - foo |
         (false ?
          atomic_fetch_add_explicit(bar,
                                    atomic_load_explicit(&x, memory_order_consume)
                                    - 27, memory_order_acquire)
          : true ? 0 : *blep))));
      barbaz = (atomic_load_explicit(&y, memory_order_acquire) ^ foo) >= (-1 & 0)
      && true;
      barbaz = *blep > *blep ||
      atomic_fetch_add_explicit(&y, false ? 0 + 0 : 0 | 0, memory_order_release) ==
      (0 >= 0 ? false ? 0 : 0 : atomic_load_explicit(bar, memory_order_seq_cst)) ||
      barbaz;
      barbaz =
      !((atomic_load_explicit(&z, memory_order_relaxed) ? 0 : 0) != (0 ^ 0));
      foo--;
      foo++;
      foo = 0;
      foo = atomic_fetch_xor_explicit(&x, 0, memory_order_acquire) ^ 0;
      (*blep)--;
      *blep = atomic_load_explicit(&y, memory_order_seq_cst) &
      ((false ? 0 <= atomic_load_explicit(bar, memory_order_seq_cst) && barbaz : 4
        >= foo)
       ? (53 & 0) - 0 :
       !(atomic_fetch_add_explicit(&y, 0, memory_order_release) > foo) ?
       atomic_fetch_xor_explicit(&x, 0 ^ 0, memory_order_relaxed) :
       atomic_fetch_or_explicit(bar, 0, memory_order_consume) > *blep ? foo : 0); |}]

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
