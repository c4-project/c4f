(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
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
    foo =
    atomic_fetch_xor_explicit(&y, 99 > *blep ? -738911604 :
                              (barbaz ?
                               atomic_fetch_and_explicit(&x, *blep - 100,
                                                         memory_order_acq_rel)
                               : atomic_load_explicit(&x, memory_order_seq_cst) -
                               27)
                              -
                              (barbaz ?
                               atomic_fetch_and_explicit(&x, *blep - 100,
                                                         memory_order_acq_rel)
                               : atomic_load_explicit(&x, memory_order_seq_cst) -
                               27),
                              memory_order_acquire);
    foo = 95 - atomic_load_explicit(bar, memory_order_consume) &
    atomic_fetch_and_explicit(&x, !barbaz ? 0 : foo - 5, memory_order_acq_rel) ^
    0 & -1;
    foo = atomic_load_explicit(&z, memory_order_seq_cst) ?
    atomic_fetch_xor_explicit(bar, *blep - 99, memory_order_acquire) - 95 ^
    atomic_fetch_sub_explicit(&x, foo - 4, memory_order_release) - 27 :
    atomic_load_explicit(&z, memory_order_relaxed) ? 0 :
    atomic_load_explicit(&x, memory_order_consume) - 27;
    foo = foo !=
    (atomic_load_explicit(&y, memory_order_relaxed) -
     atomic_load_explicit(&y, memory_order_relaxed) &
     atomic_fetch_sub_explicit(&x, *blep ^ *blep, memory_order_relaxed))
    ? atomic_load_explicit(&z, memory_order_relaxed) ?
    atomic_fetch_xor_explicit(bar,
                              !atomic_load_explicit(foobaz, memory_order_consume)
                              ?
                              atomic_fetch_add_explicit(&x,
                                                        atomic_load_explicit
                                                        (&x,
                                                         memory_order_seq_cst)
                                                        - 27,
                                                        memory_order_seq_cst)
                              :
                              atomic_fetch_sub_explicit(&y, 0,
                                                        memory_order_release)
                              - 53, memory_order_acq_rel)
    :
    atomic_fetch_xor_explicit(&y, atomic_load_explicit(bar, memory_order_relaxed)
                              - 95, memory_order_release)
    !=
    atomic_fetch_add_explicit(&y,
                              atomic_fetch_xor_explicit(&x,
                                                        atomic_fetch_xor_explicit
                                                        (bar,
                                                         atomic_fetch_add_explicit
                                                         (bar,
                                                          atomic_fetch_xor_explicit
                                                          (bar, 0,
                                                           memory_order_relaxed)
                                                          - 95,
                                                          memory_order_seq_cst)
                                                         - 95,
                                                         memory_order_relaxed)
                                                        - 95,
                                                        memory_order_acquire)
                              - 27, memory_order_seq_cst)
    - 53 ? atomic_fetch_sub_explicit(&x, 0, memory_order_acq_rel) :
    atomic_fetch_xor_explicit(bar, atomic_load_explicit(&x, memory_order_consume)
                              - 27, memory_order_acq_rel)
    :
    atomic_fetch_sub_explicit(&x,
                              !atomic_load_explicit(foobaz, memory_order_consume)
                              ? *blep : 0 ^ 0, memory_order_release);
    (*blep)--;
    (*blep)++;
    *blep = atomic_fetch_or_explicit(&x, foo - 4, memory_order_consume);
    *blep = barbaz ? foo - 4 >
    atomic_fetch_or_explicit(bar, 0, memory_order_seq_cst) ? 0 &
    atomic_load_explicit(bar, memory_order_consume) - 96 : 0 - 0 :
    atomic_load_explicit(&y, memory_order_relaxed) &
    (atomic_load_explicit(bar, memory_order_acquire) - 95 + 0 &
     (foo ^ (barbaz ? foo : atomic_load_explicit(&x, memory_order_consume)))); |}]

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
    barbaz = atomic_load_explicit(&x, memory_order_acquire) !=
    (atomic_fetch_add_explicit(&y, atomic_load_explicit(&y, memory_order_seq_cst)
                               - 53, memory_order_seq_cst)
     >=
     atomic_fetch_add_explicit(&y, atomic_load_explicit(&y, memory_order_seq_cst)
                               - 53, memory_order_seq_cst)
     ?
     atomic_fetch_sub_explicit(&y,
                               !atomic_load_explicit(foobaz,
                                                     memory_order_seq_cst)
                               ? *blep : foo - 4, memory_order_consume)
     - 53 : 53 ^ atomic_fetch_sub_explicit(&y, foo - 4, memory_order_acq_rel));
    barbaz =
    (atomic_load_explicit(bar, memory_order_acquire) ^
     ((false || 0 < foo - 4 ?
       atomic_fetch_sub_explicit(&x, 53 &
                                 atomic_fetch_and_explicit(bar,
                                                           atomic_load_explicit
                                                           (&y,
                                                            memory_order_acquire)
                                                           - 54,
                                                           memory_order_relaxed)
                                 - 95, memory_order_relaxed)
       : atomic_load_explicit(bar, memory_order_relaxed) - 95 |
       atomic_fetch_xor_explicit(bar,
                                 atomic_fetch_or_explicit(&x, foo - 4,
                                                          memory_order_release)
                                 - 27, memory_order_acquire))
      |
      (atomic_load_explicit(&x, memory_order_consume) - 28 &
       (atomic_fetch_sub_explicit(&y,
                                  atomic_fetch_xor_explicit(&y,
                                                            atomic_load_explicit
                                                            (&y,
                                                             memory_order_seq_cst)
                                                            - 53,
                                                            memory_order_seq_cst)
                                  - 53, memory_order_acquire)
        - 53 & atomic_load_explicit(bar, memory_order_relaxed) - 95)
       |
       (27 - atomic_load_explicit(&x, memory_order_relaxed) ^
        (0 <= 0 ? atomic_load_explicit(&x, memory_order_consume) - 27 : 0) |
        (!atomic_load_explicit(&z, memory_order_relaxed) ?
         atomic_fetch_xor_explicit(&y, 0, memory_order_consume) - 53 : 0)
        |
        (atomic_load_explicit(bar, memory_order_relaxed) > 0 || true ?
         atomic_fetch_and_explicit(&x, -1, memory_order_consume) - 27 |
         atomic_load_explicit(&y, memory_order_acquire) :
         atomic_fetch_add_explicit(&y, foo - 4, memory_order_seq_cst) |
         atomic_load_explicit(bar, memory_order_consume) - 95)))))
    !=
    (atomic_load_explicit(&z, memory_order_seq_cst) ?
     atomic_fetch_xor_explicit(&x, false ? *blep - 99 :
                               atomic_load_explicit(&y, memory_order_seq_cst) -
                               53, memory_order_consume)
     | atomic_fetch_xor_explicit(&x, 0, memory_order_relaxed) : 0);
    barbaz = *blep >
    (true ? atomic_load_explicit(bar, memory_order_acquire) :
     atomic_fetch_add_explicit(&x, atomic_load_explicit(&y, memory_order_acquire)
                               - 53, memory_order_consume));
    barbaz = false && atomic_load_explicit(&z, memory_order_seq_cst);
    barbaz = atomic_load_explicit(foobaz, memory_order_consume) &&
    (barbaz || atomic_load_explicit(&x, memory_order_seq_cst) >
     ((false || *blep - 99 == *blep ?
       atomic_load_explicit(&y, memory_order_seq_cst) :
       atomic_fetch_add_explicit(bar,
                                 atomic_fetch_xor_explicit(bar, 0,
                                                           memory_order_relaxed)
                                 - 95 |
                                 atomic_load_explicit(&y, memory_order_relaxed) -
                                 53, memory_order_acquire))
      |
      atomic_fetch_add_explicit(&y, (!barbaz ? 0 : *blep - 99) ^
                                atomic_fetch_and_explicit(bar,
                                                          !atomic_load_explicit
                                                          (foobaz,
                                                           memory_order_consume)
                                                          ? foo :
                                                          atomic_load_explicit
                                                          (&y,
                                                           memory_order_consume)
                                                          - 54,
                                                          memory_order_seq_cst)
                                - 95, memory_order_acq_rel)));
    barbaz = *blep != *blep &&
    (atomic_load_explicit(&x, memory_order_acquire) & 0) <
    (0 <=
     atomic_fetch_add_explicit(bar,
                               atomic_load_explicit(&y, memory_order_acquire) -
                               53, memory_order_release)
     ?
     atomic_fetch_xor_explicit(&x,
                               atomic_fetch_add_explicit(&y, foo - 4,
                                                         memory_order_consume)
                               - 53, memory_order_consume)
     : atomic_load_explicit(bar, memory_order_relaxed) - 95);
    barbaz =
    ((0 &
      (atomic_load_explicit(&y, memory_order_acquire) ^
       (atomic_load_explicit(&y, memory_order_acquire) - 53 |
        (atomic_load_explicit(foobaz, memory_order_relaxed) ?
         atomic_fetch_or_explicit(&x, *blep - 99, memory_order_consume) - 27 :
         atomic_fetch_xor_explicit(&y, foo - 4, memory_order_consume)))
       | atomic_load_explicit(&y, memory_order_acquire) &
       (atomic_fetch_or_explicit(&x, foo - 4, memory_order_consume) ^
        atomic_load_explicit(bar, memory_order_relaxed))
       &
       (atomic_fetch_xor_explicit(&y, 95 ^
                                  atomic_fetch_xor_explicit(bar, 0,
                                                            memory_order_release),
                                  memory_order_relaxed)
        & atomic_load_explicit(&y, memory_order_seq_cst) - 53 & foo)))
     <=
     (atomic_fetch_add_explicit(bar, 0, memory_order_seq_cst) > 0 ||
      (atomic_load_explicit(&z, memory_order_acquire) || *blep >
       atomic_fetch_xor_explicit(&y, foo - 4, memory_order_relaxed) - 53)
      ? 582726 : 1919995593 < *blep ?
      atomic_fetch_xor_explicit(bar,
                                atomic_load_explicit(bar, memory_order_consume) -
                                95, memory_order_acquire)
      : 0)
     ||
     (!(barbaz && barbaz) || foo <=
      (0 &
       ((true ?
         atomic_fetch_sub_explicit(&x,
                                   atomic_fetch_and_explicit(bar, -1,
                                                             memory_order_seq_cst)
                                   - 95 |
                                   atomic_load_explicit(&x, memory_order_relaxed)
                                   - 27, memory_order_consume)
         - 27 : atomic_fetch_add_explicit(&x, 0, memory_order_acquire) - 27 | foo
         - 4)
        &
        (atomic_fetch_sub_explicit(&x, 0, memory_order_seq_cst) >=
         atomic_load_explicit(&y, memory_order_seq_cst) - 53 &&
         atomic_load_explicit(&x, memory_order_relaxed) ==
         atomic_fetch_or_explicit(&y, 0, memory_order_release) - 53 ? 0 | foo - 4
         : !atomic_load_explicit(foobaz, memory_order_relaxed) ?
         atomic_fetch_or_explicit(bar, 0, memory_order_release) : foo - 4))
       &
       (atomic_load_explicit(&x, memory_order_consume) &
        (atomic_fetch_and_explicit(&y, barbaz ?
                                   atomic_fetch_sub_explicit(&y, 0,
                                                             memory_order_acquire)
                                   - 54 -
                                   (atomic_fetch_or_explicit(&y,
                                                             atomic_load_explicit
                                                             (&y,
                                                              memory_order_consume)
                                                             - 53,
                                                             memory_order_relaxed)
                                    - 53)
                                   :
                                   atomic_fetch_add_explicit(&x,
                                                             atomic_load_explicit
                                                             (foobaz,
                                                              memory_order_acquire)
                                                             ? *blep - 99 :
                                                             atomic_fetch_xor_explicit
                                                             (&x,
                                                              atomic_fetch_and_explicit
                                                              (&x, *blep - 100,
                                                               memory_order_seq_cst)
                                                              - 27,
                                                              memory_order_release),
                                                             memory_order_consume),
                                   memory_order_consume)
         & atomic_fetch_add_explicit(&x, foo - foo, memory_order_acq_rel)))
       ^
       ((barbaz ? *blep >
         atomic_fetch_xor_explicit(&y, *blep - 99, memory_order_acq_rel) ?
         atomic_load_explicit(bar, memory_order_relaxed) :
         atomic_load_explicit(&x, memory_order_relaxed) :
         atomic_fetch_sub_explicit(&x, foo - 4, memory_order_acquire))
        |
        atomic_fetch_xor_explicit(bar,
                                  atomic_fetch_add_explicit(&x, 53 -
                                                            atomic_fetch_add_explicit
                                                            (&y, true ? foo - 4 ^
                                                             *blep - 99 :
                                                             atomic_load_explicit
                                                             (bar,
                                                              memory_order_consume)
                                                             - 95 & 0,
                                                             memory_order_relaxed),
                                                            memory_order_relaxed)
                                  - 27, memory_order_acq_rel)))))
    && barbaz;
    barbaz =
    (true && atomic_load_explicit(&z, memory_order_consume) ||
     (false || atomic_load_explicit(bar, memory_order_acquire) <=
      atomic_load_explicit(bar, memory_order_consume) - 95))
    &&
    (atomic_fetch_or_explicit(bar, 0, memory_order_consume) |
     atomic_load_explicit(bar, memory_order_relaxed))
    <
    (barbaz ?
     atomic_fetch_xor_explicit(&x, atomic_load_explicit(&y, memory_order_consume)
                               - 53, memory_order_acquire)
     : atomic_load_explicit(&x, memory_order_consume) - 27);
    barbaz = atomic_load_explicit(foobaz, memory_order_consume) || true;
    barbaz = 0 > atomic_load_explicit(bar, memory_order_consume) ||
    atomic_load_explicit(&y, memory_order_acquire) ==
    atomic_fetch_or_explicit(&x, foo - 4, memory_order_acquire);
    barbaz = false ||
    !((atomic_fetch_and_explicit(&x, false ?
                                 atomic_load_explicit(bar, memory_order_seq_cst)
                                 - 95 :
                                 atomic_load_explicit(bar, memory_order_consume)
                                 - 96, memory_order_acq_rel)
       &
       atomic_fetch_and_explicit(bar, !barbaz ?
                                 atomic_fetch_and_explicit(&y,
                                                           atomic_load_explicit
                                                           (&x,
                                                            memory_order_acquire)
                                                           - 28,
                                                           memory_order_release)
                                 : -1, memory_order_release))
      >=
      ((atomic_load_explicit(bar, memory_order_relaxed) >
        atomic_load_explicit(&y, memory_order_consume) - 53 ? foo : 2249)
       ^
       (atomic_load_explicit(&x, memory_order_seq_cst) ^ *blep ^
        (atomic_load_explicit(&y, memory_order_seq_cst) - 53 <
         atomic_load_explicit(&x, memory_order_seq_cst) ? *blep - 99 : *blep))))
    || false;
    barbaz = !true;
    barbaz = !atomic_load_explicit(&z, memory_order_acquire);
    barbaz = !(*blep > atomic_load_explicit(&y, memory_order_acquire)); |}]

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
      barbaz = *blep >
      (true ? atomic_load_explicit(bar, memory_order_acquire) :
       atomic_fetch_add_explicit(&x, atomic_load_explicit(&y, memory_order_acquire)
                                 - 53, memory_order_consume));
      barbaz =
      (atomic_load_explicit(&x, memory_order_consume) &
       atomic_load_explicit(&x, memory_order_seq_cst))
      >= *blep;
      barbaz =
      (atomic_load_explicit(&x, memory_order_seq_cst) &
       (53 ^ atomic_load_explicit(&y, memory_order_relaxed)) +
       (atomic_fetch_add_explicit(bar, *blep - 99, memory_order_acq_rel) - 96 &
        *blep - 99))
      <= atomic_load_explicit(&y, memory_order_relaxed);
      barbaz = *blep != *blep &&
      (atomic_fetch_sub_explicit(bar, 0 + 0, memory_order_acquire) &
       (atomic_fetch_xor_explicit(bar, 0, memory_order_acquire) - 95 & foo - 4))
      ==
      (atomic_load_explicit(&x, memory_order_relaxed) - 27 <= foo ? 0 :
       atomic_fetch_or_explicit(&y,
                                atomic_fetch_and_explicit(bar, foo - 5,
                                                          memory_order_relaxed)
                                - 95, memory_order_acq_rel));
      barbaz =
      ((atomic_load_explicit(bar, memory_order_consume) > 0 ? foo :
        atomic_load_explicit(bar, memory_order_consume) - 95)
       | atomic_fetch_add_explicit(&x, foo - 4, memory_order_acquire) &
       atomic_load_explicit(&y, memory_order_relaxed) - 53)
      <= 0 || barbaz;
      barbaz = barbaz &&
      ((atomic_load_explicit(&y, memory_order_acquire) ==
        atomic_fetch_xor_explicit(&y,
                                  atomic_load_explicit(bar, memory_order_relaxed) -
                                  95, memory_order_seq_cst)
        ? atomic_load_explicit(bar, memory_order_seq_cst) ^
        atomic_fetch_xor_explicit(bar,
                                  atomic_fetch_add_explicit(&x, 0,
                                                            memory_order_acquire)
                                  - 27, memory_order_acq_rel)
        : atomic_load_explicit(&z, memory_order_relaxed) ?
        atomic_fetch_xor_explicit(&x,
                                  atomic_fetch_and_explicit(bar,
                                                            atomic_fetch_add_explicit
                                                            (&x, *blep - 99,
                                                             memory_order_relaxed)
                                                            - 28,
                                                            memory_order_acquire)
                                  - 95, memory_order_acq_rel)
        : atomic_load_explicit(&y, memory_order_seq_cst))
       ^
       (atomic_fetch_add_explicit(bar, 27 -
                                  atomic_load_explicit(&x, memory_order_relaxed) -
                                  (*blep - 99), memory_order_acq_rel)
        | *blep)
       & atomic_load_explicit(&x, memory_order_relaxed) |
       (atomic_fetch_and_explicit(&y,
                                  atomic_load_explicit(foobaz,
                                                       memory_order_seq_cst)
                                  ? barbaz ?
                                  atomic_load_explicit(&x, memory_order_consume) -
                                  28 :
                                  atomic_load_explicit(bar, memory_order_relaxed) :
                                  -3 ^ -3, memory_order_release)
        |
        (false && barbaz ? foo >= foo ?
         atomic_fetch_xor_explicit(&x, 0, memory_order_release) :
         atomic_fetch_sub_explicit(&x, 0, memory_order_consume) :
         atomic_fetch_or_explicit(bar,
                                  atomic_fetch_xor_explicit(&y, 0,
                                                            memory_order_consume)
                                  - 53, memory_order_relaxed)
         | atomic_fetch_and_explicit(&x, *blep - 100, memory_order_relaxed))))
      == atomic_fetch_xor_explicit(&x, 0, memory_order_relaxed) ||
      atomic_load_explicit(foobaz, memory_order_acquire);
      barbaz = false ||
      !((atomic_fetch_and_explicit(&x, false ?
                                   atomic_load_explicit(bar, memory_order_seq_cst)
                                   - 95 :
                                   atomic_load_explicit(bar, memory_order_consume)
                                   - 96, memory_order_acq_rel)
         &
         atomic_fetch_and_explicit(bar, !barbaz ?
                                   atomic_fetch_and_explicit(&y,
                                                             atomic_load_explicit
                                                             (&x,
                                                              memory_order_acquire)
                                                             - 28,
                                                             memory_order_release)
                                   : -1, memory_order_release))
        >=
        ((atomic_load_explicit(bar, memory_order_relaxed) >
          atomic_load_explicit(&y, memory_order_consume) - 53 ? foo : 2249)
         ^
         (atomic_load_explicit(&x, memory_order_seq_cst) ^ *blep ^
          (atomic_load_explicit(&y, memory_order_seq_cst) - 53 <
           atomic_load_explicit(&x, memory_order_seq_cst) ? *blep - 99 : *blep))))
      || false;
      foo--;
      foo =
      atomic_fetch_add_explicit(bar,
                                atomic_load_explicit(bar, memory_order_acquire) -
                                95, memory_order_release);
      foo =
      atomic_fetch_and_explicit(&x,
                                !atomic_load_explicit(foobaz, memory_order_relaxed)
                                ? -52859389 : -1, memory_order_relaxed);
      foo = atomic_load_explicit(bar, memory_order_consume) - 95;
      foo = atomic_load_explicit(bar, memory_order_relaxed) &
      (((atomic_load_explicit(&z, memory_order_acquire) ?
         atomic_load_explicit(&y, memory_order_consume) : 0)
        ^ atomic_load_explicit(&x, memory_order_relaxed))
       -
       ((atomic_load_explicit(&z, memory_order_acquire) ?
         atomic_load_explicit(&y, memory_order_consume) : 0)
        ^ atomic_load_explicit(&x, memory_order_relaxed))
       &
       (0 & atomic_load_explicit(&x, memory_order_seq_cst) &
        (atomic_fetch_sub_explicit(&y, 0, memory_order_release) | *blep - 99) ^ 0 &
        atomic_load_explicit(&x, memory_order_seq_cst) &
        (atomic_fetch_sub_explicit(&y, 0, memory_order_release) | *blep - 99)));
      (*blep)--;
      (*blep)++;
      *blep = *blep & (-738911604 | foo) ^ *blep & (-738911604 | foo);
      *blep = atomic_load_explicit(foobaz, memory_order_consume) ?
      atomic_fetch_and_explicit(&y, -1, memory_order_release) ^
      atomic_load_explicit(bar, memory_order_seq_cst) ^
      (atomic_fetch_and_explicit(&y, -1, memory_order_release) ^
       atomic_load_explicit(bar, memory_order_seq_cst))
      :
      atomic_fetch_xor_explicit(bar, 2147483647 - 2147483647 - (true ? 0 : -60),
                                memory_order_release); |}]

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
