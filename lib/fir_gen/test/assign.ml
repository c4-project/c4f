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
    (*blep)--;
    (*blep)++;
    *blep = atomic_load_explicit(bar, memory_order_relaxed);
    *blep = atomic_load_explicit(&x, memory_order_relaxed);
    *blep = (*blep & *blep) - (*blep & *blep);
    *blep = -1 &
    (false ? atomic_fetch_add_explicit(&x, 0, memory_order_release) | 0 : 53 & 0);
    *blep = 27 - atomic_load_explicit(&x, memory_order_relaxed) |
    ((true ? false : 0 <= 0) ?
     (true ? 0 : atomic_fetch_or_explicit(bar, 0, memory_order_release)) ^
     (0 ^ atomic_fetch_add_explicit(&x, 0, memory_order_acquire)) : true ? 27 & 0
     : atomic_fetch_and_explicit(&y, -1, memory_order_seq_cst) | 0)
    & (0 | (false ? atomic_fetch_or_explicit(bar, 0, memory_order_seq_cst) : 0));
    *blep = 95 - atomic_load_explicit(bar, memory_order_consume) &
    atomic_fetch_and_explicit(&x, false ?
                              atomic_fetch_or_explicit(&x, 0,
                                                       memory_order_acquire)
                              : -1, memory_order_acq_rel)
    ^
    atomic_fetch_add_explicit(&y, false ? 0 |
                              atomic_fetch_add_explicit(&y, 0,
                                                        memory_order_acquire)
                              : 0 ^ 0, memory_order_consume);
    *blep =
    (*blep > 99 ?
     (true && true ? true ? 99 - *blep :
      atomic_load_explicit(&x, memory_order_acquire) & 0 :
      atomic_fetch_add_explicit(bar, 0, memory_order_release) -
      atomic_fetch_add_explicit(bar, 0, memory_order_release) ^
      atomic_load_explicit(&y, memory_order_consume))
     - 0 : 0 +
     (27 - atomic_fetch_add_explicit(&x, 4 - foo, memory_order_consume)))
    ^
    (foo ^
     (atomic_load_explicit(&y, memory_order_relaxed) -
      atomic_load_explicit(&y, memory_order_relaxed) ==
      atomic_load_explicit(&y, memory_order_relaxed) -
      atomic_load_explicit(&y, memory_order_relaxed) ? (4 & 0) - (4 & 0) :
      atomic_fetch_and_explicit(&y, -1, memory_order_relaxed) &
      atomic_load_explicit(&x, memory_order_relaxed) -
      atomic_load_explicit(&x, memory_order_relaxed)));
    *blep = atomic_load_explicit(&x, memory_order_seq_cst) >
    atomic_load_explicit(&x, memory_order_seq_cst) ?
    atomic_load_explicit(bar, memory_order_seq_cst) ^
    ((atomic_load_explicit(bar, memory_order_relaxed) | 4 - foo) &
     atomic_fetch_and_explicit(&y, 95 <
                               atomic_fetch_and_explicit(bar,
                                                         (false ?
                                                          atomic_fetch_add_explicit
                                                          (&x, 0,
                                                           memory_order_seq_cst)
                                                          : -1)
                                                         | 0,
                                                         memory_order_consume)
                               ? foo - foo |
                               atomic_load_explicit(&y, memory_order_relaxed) ^
                               atomic_load_explicit(&x, memory_order_consume) :
                               (*blep ^ 0) > (*blep ^ 0) ?
                               atomic_load_explicit(&y, memory_order_acquire) :
                               (false ? foo : -1) & -1, memory_order_seq_cst)
     |
     (atomic_fetch_sub_explicit(bar, 0 + (0 & -1), memory_order_release) <= 95 ?
      true && (0 != 0 && false) ? atomic_load_explicit(&x, memory_order_acquire)
      : foo & 0 :
      atomic_fetch_add_explicit(bar, 27 ^
                                atomic_load_explicit(&x, memory_order_relaxed),
                                memory_order_relaxed)))
    : false && true ?
    atomic_fetch_and_explicit(&y, (-1 | -1) - 0 ^ 0, memory_order_relaxed) |
    (*blep & 10 ^ *blep & 10 | 0) : -1 & (0 & 0 ^ 0); |}]

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
    barbaz = *blep ==
    ((true ? true ? 0 : atomic_fetch_and_explicit(&y, -1, memory_order_acq_rel) :
      0 | 0)
     | (0 | 0 | 2147483647));
    barbaz =
    (true || false || false ? 99 <= *blep ? 0 & 99 :
     (atomic_load_explicit(&z, memory_order_consume) || true ? (0 & -1) -
      (0 & -1) : atomic_load_explicit(&x, memory_order_consume))
     &
     atomic_fetch_sub_explicit(&x, 27 -
                               atomic_load_explicit(&x, memory_order_relaxed) -
                               (27 -
                                atomic_load_explicit(&x, memory_order_relaxed)),
                               memory_order_acq_rel)
     &
     ((foo | atomic_fetch_or_explicit(&y, 0, memory_order_seq_cst)) !=
      (foo | atomic_fetch_or_explicit(&y, 0, memory_order_seq_cst)) ? 0 & 0 & foo
      : 0)
     :
     atomic_fetch_xor_explicit(&y, 0 &
                               (*blep & 0 &
                                atomic_fetch_and_explicit(&y, false ?
                                                          atomic_fetch_xor_explicit
                                                          (&x, 0,
                                                           memory_order_acq_rel)
                                                          - 27 : false ?
                                                          atomic_fetch_xor_explicit
                                                          (&y, 0,
                                                           memory_order_consume)
                                                          : -1,
                                                          memory_order_relaxed)),
                               memory_order_acq_rel))
    >= 690;
    barbaz =
    atomic_fetch_xor_explicit(&x, atomic_load_explicit(&x, memory_order_relaxed)
                              != 27 ? true ?
                              atomic_fetch_add_explicit(&x,
                                                        atomic_load_explicit
                                                        (&y,
                                                         memory_order_consume)
                                                        <= 53 ? 0 & 95 :
                                                        atomic_fetch_xor_explicit
                                                        (&x,
                                                         atomic_fetch_and_explicit
                                                         (bar, true ? -1 :
                                                          atomic_fetch_or_explicit
                                                          (bar, 0,
                                                           memory_order_seq_cst),
                                                          memory_order_seq_cst)
                                                         ^
                                                         atomic_fetch_and_explicit
                                                         (bar, true ? -1 :
                                                          atomic_fetch_or_explicit
                                                          (bar, 0,
                                                           memory_order_seq_cst),
                                                          memory_order_seq_cst),
                                                         memory_order_acquire),
                                                        memory_order_acq_rel)
                              - 27 :
                              atomic_fetch_and_explicit(&y, -1,
                                                        memory_order_seq_cst)
                              : 95 <
                              atomic_fetch_and_explicit(bar,
                                                        atomic_fetch_or_explicit
                                                        (&y, 0,
                                                         memory_order_consume)
                                                        >= 53 ? 0 | -1 : -1,
                                                        memory_order_relaxed)
                              ?
                              atomic_fetch_and_explicit(&y, true ? true ? -1 : 0
                                                        :
                                                        atomic_fetch_sub_explicit
                                                        (&y, 4 & 0,
                                                         memory_order_acquire),
                                                        memory_order_consume)
                              ^
                              atomic_fetch_and_explicit(&y, true ? true ? -1 : 0
                                                        :
                                                        atomic_fetch_sub_explicit
                                                        (&y, 4 & 0,
                                                         memory_order_acquire),
                                                        memory_order_consume)
                              :
                              (barbaz && false ?
                               !atomic_load_explicit(foobaz,
                                                     memory_order_acquire)
                               :
                               atomic_load_explicit(foobaz, memory_order_acquire)
                               && false)
                              ? 0 - 0 - (0 - 0) : 0, memory_order_release)
    <
    (((atomic_load_explicit(bar, memory_order_acquire) ^ 95) - 0 |
      (0 & 0 ^ *blep | atomic_fetch_add_explicit(&y, 0, memory_order_consume) -
       atomic_fetch_add_explicit(&y, 0, memory_order_consume)))
     & *blep ^ 0 ^
     (atomic_fetch_and_explicit(&y, foo != 4 ?
                                atomic_load_explicit(&x, memory_order_consume) :
                                -1, memory_order_acquire)
      ^
      ((true ? barbaz != barbaz : barbaz || barbaz) ?
       atomic_fetch_sub_explicit(&x, 0, memory_order_consume) & 0 |
       atomic_fetch_and_explicit(bar, -1, memory_order_acquire) ^
       atomic_fetch_and_explicit(bar, -1, memory_order_acquire) : 95 ^
       atomic_fetch_xor_explicit(bar, 0 | 0, memory_order_relaxed))
      & foo));
    barbaz =
    (0 &
     atomic_fetch_and_explicit(bar,
                               (true ?
                                atomic_load_explicit(&z, memory_order_relaxed) &&
                                false :
                                !atomic_load_explicit(&z, memory_order_seq_cst))
                               ?
                               atomic_fetch_or_explicit(&x,
                                                        atomic_fetch_add_explicit
                                                        (bar, 0,
                                                         memory_order_release)
                                                        ^ 95,
                                                        memory_order_consume)
                               | foo |
                               (0 |
                                atomic_load_explicit(bar, memory_order_consume) |
                                atomic_fetch_add_explicit(&y, true ? 0 :
                                                          atomic_load_explicit
                                                          (&y,
                                                           memory_order_seq_cst),
                                                          memory_order_relaxed))
                               : 27 >=
                               atomic_fetch_or_explicit(&x, 0,
                                                        memory_order_acq_rel)
                               ? true ? true ? -1 :
                               atomic_fetch_or_explicit(bar, 0,
                                                        memory_order_release)
                               : true ? 0 : 0 : 0 ^ 0 |
                               atomic_fetch_xor_explicit(&x, 95 ^
                                                         atomic_load_explicit
                                                         (bar,
                                                          memory_order_seq_cst),
                                                         memory_order_relaxed),
                               memory_order_acq_rel)
     ^
     (atomic_fetch_and_explicit(&x,
                                atomic_load_explicit(foobaz,
                                                     memory_order_seq_cst)
                                <= true ?
                                atomic_load_explicit(&x, memory_order_consume) !=
                                27 ? 99 - *blep : 0 | 0 + (false ? 1048576 : -1)
                                :
                                atomic_fetch_sub_explicit(bar, 0 & 27 ^ 0 & 27,
                                                          memory_order_consume),
                                memory_order_seq_cst)
      | *blep)
     ^ atomic_load_explicit(&x, memory_order_seq_cst) | *blep)
    < (atomic_fetch_xor_explicit(&x, 0, memory_order_seq_cst) ^ 0);
    barbaz = atomic_load_explicit(foobaz, memory_order_consume) &&
    (barbaz || atomic_load_explicit(&x, memory_order_seq_cst) !=
     (atomic_load_explicit(bar, memory_order_seq_cst) ^
      ((0 | 0) & atomic_fetch_sub_explicit(&y, 0 & -1, memory_order_seq_cst) |
       atomic_fetch_sub_explicit(bar, 0 & -1, memory_order_acq_rel))
      | 0 + (0 | 0) & foo));
    barbaz = (atomic_fetch_add_explicit(&y, 0 & 0, memory_order_relaxed) & 0) <=
    (0 + 0 & atomic_fetch_or_explicit(&y, 0, memory_order_acq_rel) -
     atomic_fetch_or_explicit(&y, 0, memory_order_acq_rel))
    && *blep > *blep;
    barbaz = atomic_load_explicit(foobaz, memory_order_consume) &&
    (barbaz && false ?
     (*blep | atomic_load_explicit(&x, memory_order_relaxed) &
      (atomic_fetch_sub_explicit(&x, 0, memory_order_consume) &
       atomic_load_explicit(bar, memory_order_relaxed)))
     & foo : atomic_load_explicit(bar, memory_order_relaxed) >
     atomic_load_explicit(bar, memory_order_relaxed) ?
     atomic_fetch_or_explicit(&x, false ?
                              atomic_fetch_or_explicit(bar,
                                                       atomic_fetch_and_explicit
                                                       (bar, -1,
                                                        memory_order_consume)
                                                       -
                                                       atomic_fetch_and_explicit
                                                       (bar, -1,
                                                        memory_order_consume),
                                                       memory_order_consume)
                              : 0 & 0, memory_order_seq_cst)
     |
     (atomic_fetch_and_explicit(&x, -1, memory_order_consume) & 0 | *blep ^
      atomic_fetch_add_explicit(&y, 0, memory_order_seq_cst))
     : 0 & atomic_fetch_and_explicit(bar, -1 & -1, memory_order_seq_cst))
    == *blep - 99 &&
    (atomic_load_explicit(&x, memory_order_consume) &
     (foo - 4 ^ (atomic_load_explicit(bar, memory_order_seq_cst) ^ 95)) ^
     atomic_load_explicit(bar, memory_order_relaxed) - 95)
    > foo;
    barbaz =
    ((0 <= atomic_fetch_add_explicit(&y, 0, memory_order_acquire) || false) &&
     (atomic_fetch_add_explicit(&x, 0, memory_order_relaxed) <= 0 &&
      atomic_load_explicit(bar, memory_order_relaxed) == 0)
     || atomic_load_explicit(foobaz, memory_order_relaxed))
    &&
    (!barbaz || !(atomic_fetch_xor_explicit(&x, 0, memory_order_relaxed) > 0));
    barbaz = 0 > atomic_load_explicit(bar, memory_order_consume) ||
    atomic_load_explicit(&y, memory_order_acquire) !=
    atomic_fetch_or_explicit(&x, 0, memory_order_acquire);
    barbaz = atomic_load_explicit(&y, memory_order_consume) <
    (53 - atomic_load_explicit(&y, memory_order_consume) & 1) ||
    !(false ||
      !((atomic_fetch_and_explicit(bar, 0 + -1, memory_order_acq_rel) &
         atomic_fetch_add_explicit(bar, 95 & 0, memory_order_release))
        <= *blep)
      || true &&
      ((!((barbaz || foo != foo) &&
          (false && atomic_load_explicit(&x, memory_order_consume) >
           atomic_fetch_or_explicit(bar, 0, memory_order_acq_rel)))
        || true || foo < (1035516393 - 1035516393 ^ 0 + 0))
       && *blep >= atomic_load_explicit(bar, memory_order_seq_cst) - 95)
      || atomic_load_explicit(&x, memory_order_relaxed) ==
      (0 & atomic_fetch_or_explicit(&x, -1 & 0, memory_order_acq_rel) & *blep) ||
      (atomic_fetch_xor_explicit(&x,
                                 (atomic_load_explicit(bar, memory_order_relaxed)
                                  ^ 0)
                                 -
                                 (atomic_load_explicit(bar, memory_order_relaxed)
                                  ^ 0),
                                 memory_order_acquire)
       | foo)
      >
      (atomic_fetch_sub_explicit(&y, 0 |
                                 (false ?
                                  atomic_fetch_sub_explicit(bar, 0,
                                                            memory_order_acquire)
                                  : 0),
                                 memory_order_consume)
       |
       (atomic_load_explicit(bar, memory_order_acquire) |
        (true ? 0 : atomic_fetch_or_explicit(bar, 0, memory_order_release)) ^
        (0 ^ atomic_fetch_add_explicit(&x, 0, memory_order_acquire)) |
        atomic_fetch_and_explicit(&y, -1 & -1 | 0, memory_order_consume))));
    barbaz = !atomic_load_explicit(&z, memory_order_acquire);
    barbaz =
    !(*blep !=
      (atomic_fetch_or_explicit(&x, 0 &
                                atomic_load_explicit(bar, memory_order_consume),
                                memory_order_acq_rel)
       & atomic_load_explicit(&x, memory_order_consume)));
    barbaz =
    !(foo <=
      atomic_fetch_add_explicit(bar,
                                atomic_load_explicit(&y, memory_order_relaxed) -
                                53, memory_order_release));
    barbaz =
    !((true && barbaz && atomic_load_explicit(&z, memory_order_seq_cst) ||
       (atomic_load_explicit(&z, memory_order_seq_cst) && false || false))
      &&
      !(((atomic_load_explicit(bar, memory_order_seq_cst) & 0) ==
         (atomic_load_explicit(bar, memory_order_seq_cst) & 0) ? 53 & 0 :
         atomic_fetch_xor_explicit(&y, 0 &
                                   atomic_load_explicit(&x, memory_order_consume),
                                   memory_order_seq_cst))
        >
        ((53 > atomic_fetch_sub_explicit(&y, 0 - 0, memory_order_acquire) ? 0 &
          -1 | atomic_fetch_and_explicit(bar, 0 + -1, memory_order_acquire) |
          atomic_fetch_xor_explicit(bar, 53 -
                                    atomic_load_explicit(&y,
                                                         memory_order_consume),
                                    memory_order_seq_cst)
          : (0 | 0) - 0)
         & (atomic_load_explicit(&y, memory_order_consume) & 0) ^ foo))
      && !(barbaz && !barbaz)); |}]

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
      barbaz = *blep == 2147483647;
      barbaz =
      (atomic_fetch_xor_explicit(bar, 0 & 0, memory_order_consume) | 0 &
       atomic_fetch_add_explicit(&x, 0, memory_order_seq_cst))
      != (foo ^ atomic_fetch_xor_explicit(bar, -1 & 0, memory_order_release));
      barbaz =
      ((atomic_load_explicit(bar, memory_order_relaxed) | 4 - foo) &
       atomic_fetch_and_explicit(bar, 95 <
                                 atomic_fetch_and_explicit(bar,
                                                           (false ?
                                                            atomic_fetch_add_explicit
                                                            (&x, 0,
                                                             memory_order_seq_cst)
                                                            : -1)
                                                           | 0,
                                                           memory_order_consume)
                                 ? foo - foo |
                                 atomic_load_explicit(&y, memory_order_relaxed) ^
                                 atomic_load_explicit(&x, memory_order_consume) :
                                 (*blep ^ 0) > (*blep ^ 0) ?
                                 atomic_load_explicit(&y, memory_order_acquire) : 0
                                 ^
                                 (true ? -1 :
                                  atomic_load_explicit(&y, memory_order_consume)),
                                 memory_order_seq_cst)
       |
       atomic_fetch_or_explicit(bar,
                                atomic_fetch_sub_explicit(&x, 0,
                                                          memory_order_release)
                                == 27 ? false ?
                                atomic_load_explicit(&y, memory_order_seq_cst) :
                                atomic_load_explicit(&x, memory_order_seq_cst) -
                                atomic_load_explicit(&x, memory_order_seq_cst) :
                                atomic_load_explicit(bar, memory_order_seq_cst),
                                memory_order_acq_rel)
       & 0)
      <= atomic_fetch_or_explicit(&y, 0 + (foo ^ 4), memory_order_acquire);
      barbaz = 0 <
      (atomic_fetch_sub_explicit(bar, 0 + 0, memory_order_acq_rel) &
       atomic_load_explicit(&x, memory_order_consume) ^
       (atomic_load_explicit(bar, memory_order_relaxed) ^ 95));
      barbaz = barbaz &&
      !(atomic_fetch_and_explicit(&y, (-1 | -1) - 0 ^ 0, memory_order_relaxed) <
        (*blep & 10 ^ *blep & 10 | 0));
      barbaz = (atomic_fetch_add_explicit(&y, 0 & 0, memory_order_relaxed) & 0) >=
      (0 + (0 & 27) & (*blep ^ foo)) && ((-1 & 0) - 0 & 0) >
      atomic_load_explicit(&y, memory_order_relaxed) - 53;
      barbaz = (atomic_load_explicit(&y, memory_order_acquire) ^ foo) >= (-1 & 0)
      && true;
      barbaz = !atomic_load_explicit(&z, memory_order_acquire);
      barbaz = !(*blep > atomic_load_explicit(&y, memory_order_acquire));
      foo--;
      foo++;
      foo = 0;
      foo = atomic_fetch_xor_explicit(&x, 0, memory_order_acquire) ^ 0;
      (*blep)--;
      (*blep)++;
      *blep = *blep;
      *blep =
      atomic_fetch_add_explicit(bar, 95 -
                                atomic_load_explicit(bar, memory_order_seq_cst),
                                memory_order_acq_rel);
      *blep = atomic_load_explicit(bar, memory_order_consume); |}]

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
