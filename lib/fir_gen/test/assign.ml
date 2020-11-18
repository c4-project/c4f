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
    foo =
    (*blep > 99 ?
     (true && true ? true ? 99 - *blep :
      atomic_load_explicit(&x, memory_order_acquire) & 0 : false && true ?
      atomic_load_explicit(&x, memory_order_relaxed) |
      atomic_fetch_xor_explicit(&y, 0, memory_order_consume) : *blep)
     + 0 : 4 - foo)
    ^ -1 & (0 & atomic_load_explicit(&y, memory_order_relaxed)) -
    (0 & atomic_load_explicit(&y, memory_order_relaxed));
    foo =
    (53 >=
     atomic_fetch_xor_explicit(&y,
                               atomic_fetch_xor_explicit(bar,
                                                         atomic_fetch_xor_explicit
                                                         (&y, 0,
                                                          memory_order_consume)
                                                         ==
                                                         atomic_fetch_xor_explicit
                                                         (&y, 0,
                                                          memory_order_consume)
                                                         ?
                                                         atomic_load_explicit
                                                         (&x,
                                                          memory_order_seq_cst)
                                                         ^
                                                         atomic_load_explicit
                                                         (&x,
                                                          memory_order_seq_cst)
                                                         : 0 <=
                                                         atomic_fetch_sub_explicit
                                                         (bar, 0,
                                                          memory_order_release)
                                                         || 0 >= 0 ? 4 - foo :
                                                         (0 |
                                                          atomic_fetch_xor_explicit
                                                          (bar, 0,
                                                           memory_order_relaxed))
                                                         ^ 0 &
                                                         atomic_fetch_xor_explicit
                                                         (bar, 0,
                                                          memory_order_relaxed),
                                                         memory_order_acq_rel)
                               & 0, memory_order_relaxed)
     ? barbaz == true :
     !(*blep !=
       (atomic_fetch_or_explicit(&x, 0 &
                                 atomic_load_explicit(bar, memory_order_consume),
                                 memory_order_acq_rel)
        & atomic_load_explicit(&x, memory_order_consume))))
    ? 99 ^ *blep :
    ((foo & atomic_load_explicit(&y, memory_order_consume)) <=
     (1 | (atomic_fetch_and_explicit(&x, -1, memory_order_acq_rel) < 0 ? 0 : 0))
     ? true == barbaz ? 0 & foo : (barbaz || 0 < 0) &&
     (barbaz || atomic_load_explicit(bar, memory_order_consume) <
      atomic_fetch_and_explicit(&y, -1, memory_order_seq_cst))
     ? atomic_fetch_add_explicit(&y, 4 - foo, memory_order_release) : true ?
     atomic_fetch_or_explicit(&x, 0, memory_order_seq_cst) ^
     atomic_fetch_or_explicit(&x, 0, memory_order_seq_cst) :
     atomic_load_explicit(&x, memory_order_seq_cst) : *blep)
    <=
    ((0 ^ 1035516393) >
     atomic_fetch_and_explicit(&y, 0 + -1, memory_order_seq_cst) || barbaz ?
     atomic_load_explicit(&y, memory_order_relaxed) : 0 &
     atomic_load_explicit(bar, memory_order_consume))
    &&
    (!!true ||
     ((atomic_fetch_sub_explicit(&y,
                                 atomic_fetch_or_explicit(bar,
                                                          atomic_load_explicit
                                                          (&x,
                                                           memory_order_relaxed)
                                                          - 27,
                                                          memory_order_seq_cst)
                                 - 95, memory_order_acq_rel)
       ^ 0 & 0)
      & atomic_load_explicit(bar, memory_order_seq_cst))
     <
     ((atomic_fetch_or_explicit(&y, 0, memory_order_seq_cst) &
       atomic_load_explicit(bar, memory_order_seq_cst))
      <= atomic_fetch_sub_explicit(&y, 0 + 0, memory_order_acq_rel) ?
      atomic_load_explicit(bar, memory_order_acquire) |
      (0 < atomic_fetch_or_explicit(bar, 0, memory_order_relaxed) ? *blep :
       atomic_fetch_and_explicit(&y, -1, memory_order_release))
      | atomic_load_explicit(&x, memory_order_consume) :
      (!true ? atomic_fetch_and_explicit(&y, -1, memory_order_seq_cst) | 0 :
       atomic_load_explicit(&x, memory_order_consume) | 0)
      ^
      (0 | *blep | atomic_fetch_add_explicit(bar, 0 | 0, memory_order_acq_rel))))
    ?
    (true || barbaz ? 99 ^ *blep :
     (atomic_fetch_and_explicit(&x, true > barbaz ?
                                (atomic_fetch_add_explicit(&x, 0,
                                                           memory_order_release)
                                 &
                                 atomic_load_explicit(bar, memory_order_relaxed)
                                 | atomic_load_explicit(&x, memory_order_relaxed)
                                 ^ 27)
                                <= (95 & 0) ?
                                atomic_fetch_or_explicit(&x,
                                                         (false ? 0 ==
                                                          atomic_fetch_and_explicit
                                                          (&x, -1,
                                                           memory_order_release)
                                                          : true)
                                                         ? false ? foo | *blep :
                                                         99 - *blep : foo - 4,
                                                         memory_order_consume)
                                &
                                atomic_fetch_xor_explicit(&x, 27 & 0,
                                                          memory_order_seq_cst)
                                ^
                                (27 ^
                                 atomic_fetch_or_explicit(&x,
                                                          (atomic_load_explicit
                                                           (&y,
                                                            memory_order_acquire)
                                                           |
                                                           atomic_load_explicit
                                                           (&x,
                                                            memory_order_relaxed))
                                                          -
                                                          (atomic_load_explicit
                                                           (&y,
                                                            memory_order_acquire)
                                                           |
                                                           atomic_load_explicit
                                                           (&x,
                                                            memory_order_relaxed)),
                                                          memory_order_acq_rel))
                                :
                                atomic_fetch_add_explicit(&x,
                                                          atomic_load_explicit
                                                          (&y,
                                                           memory_order_consume)
                                                          <= 53 ? 53 & 0 :
                                                          atomic_fetch_or_explicit
                                                          (bar, 0,
                                                           memory_order_consume)
                                                          ^
                                                          atomic_load_explicit
                                                          (&x,
                                                           memory_order_relaxed),
                                                          memory_order_acq_rel)
                                :
                                (false ? 0 >
                                 atomic_fetch_or_explicit(&x, 0,
                                                          memory_order_consume)
                                 ? 0 : *blep : 0 | -1)
                                & (false ? 0 & 0 : false ? 0 : -1),
                                memory_order_release)
      | atomic_load_explicit(&y, memory_order_acquire))
     &
     (atomic_fetch_or_explicit(&x, 0 & (0 & 95 ^ foo), memory_order_consume) ^
      (atomic_fetch_and_explicit(&y,
                                 (true ? true ? -1 :
                                  atomic_load_explicit(&y, memory_order_consume)
                                  :
                                  atomic_fetch_or_explicit(bar, 0,
                                                           memory_order_seq_cst)
                                  &
                                  atomic_fetch_and_explicit(&y, -1,
                                                            memory_order_relaxed))
                                 &
                                 (true ? -1 : 0 <=
                                  atomic_load_explicit(&x, memory_order_acquire)
                                  ?
                                  atomic_fetch_and_explicit(&y, -1,
                                                            memory_order_relaxed)
                                  :
                                  atomic_fetch_add_explicit(bar, 0,
                                                            memory_order_seq_cst)),
                                 memory_order_release)
       ^ *blep)
      & atomic_load_explicit(&y, memory_order_relaxed)))
    &
    (atomic_fetch_or_explicit(&x, barbaz > barbaz ? foo |
                              atomic_fetch_or_explicit(bar,
                                                       (true && true ? 4 < foo :
                                                        (0 |
                                                         atomic_fetch_and_explicit
                                                         (&y, -1,
                                                          memory_order_seq_cst))
                                                        <= 0)
                                                       ?
                                                       (atomic_fetch_sub_explicit
                                                        (&x, 0,
                                                         memory_order_consume)
                                                        & 0 |
                                                        atomic_fetch_and_explicit
                                                        (bar, -1,
                                                         memory_order_acquire)
                                                        ^
                                                        atomic_fetch_and_explicit
                                                        (bar, -1,
                                                         memory_order_acquire))
                                                       -
                                                       (atomic_fetch_sub_explicit
                                                        (&x, 0,
                                                         memory_order_consume)
                                                        & 0 |
                                                        atomic_fetch_and_explicit
                                                        (bar, -1,
                                                         memory_order_acquire)
                                                        ^
                                                        atomic_fetch_and_explicit
                                                        (bar, -1,
                                                         memory_order_acquire))
                                                       : 53 ^
                                                       atomic_fetch_add_explicit
                                                       (&y, 0 &
                                                        (0 > 0 ?
                                                         atomic_fetch_sub_explicit
                                                         (&y, 0,
                                                          memory_order_release)
                                                         : 0),
                                                        memory_order_acq_rel),
                                                       memory_order_acquire)
                              : true < barbaz ?
                              (atomic_load_explicit(&z, memory_order_seq_cst) ?
                               -1 & (0 & 0) ^
                               (0 == 0 ? false ?
                                atomic_fetch_add_explicit(bar, 0,
                                                          memory_order_seq_cst)
                                & 0 : 0 - 0 : 27 & 0)
                               :
                               atomic_fetch_and_explicit(&y, (true ? -1 : 0) ^ 0,
                                                         memory_order_consume))
                              &
                              (atomic_fetch_or_explicit(bar, false ?
                                                        atomic_fetch_xor_explicit
                                                        (bar, 0,
                                                         memory_order_consume)
                                                        : 0,
                                                        memory_order_seq_cst)
                               ^
                               atomic_fetch_or_explicit(bar, false ?
                                                        atomic_fetch_xor_explicit
                                                        (bar, 0,
                                                         memory_order_consume)
                                                        : 0,
                                                        memory_order_seq_cst)
                               ^
                               (false || foo >=
                                atomic_load_explicit(&y, memory_order_consume) &&
                                (atomic_load_explicit(&z, memory_order_relaxed)
                                 || 0 <
                                 atomic_fetch_xor_explicit(bar, 0,
                                                           memory_order_release))
                                ? atomic_load_explicit(&y, memory_order_consume)
                                & 0 ^
                                atomic_fetch_add_explicit(bar,
                                                          atomic_fetch_or_explicit
                                                          (bar, 0 + 0,
                                                           memory_order_consume)
                                                          - 95,
                                                          memory_order_seq_cst)
                                : *blep))
                              : -1 & foo - 4, memory_order_release)
     - 27 | 0 & *blep)
    : atomic_fetch_xor_explicit(bar, foo & 0, memory_order_acq_rel);
    (*blep)--;
    (*blep)++;
    *blep = atomic_fetch_xor_explicit(&x, 4 - foo - 0, memory_order_acq_rel);
    *blep = (*blep & *blep) - (*blep & *blep);
    *blep = -1 &
    (false ? atomic_fetch_add_explicit(&x, 0, memory_order_release) | 0 : 53 & 0);
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
    *blep = atomic_load_explicit(&z, memory_order_consume) != false ?
    atomic_fetch_and_explicit(&x,
                              ((atomic_fetch_and_explicit(&y, -1,
                                                          memory_order_acq_rel)
                                & 0)
                               != 0)
                              <=
                              ((atomic_fetch_and_explicit(&y, -1,
                                                          memory_order_acq_rel)
                                & 0)
                               != 0)
                              || false ? false ? 0 &
                              atomic_load_explicit(&y, memory_order_consume) &
                              atomic_load_explicit(bar, memory_order_relaxed) :
                              atomic_load_explicit(&z, memory_order_acquire) !=
                              atomic_load_explicit(&z, memory_order_acquire) ?
                              atomic_load_explicit(&y, memory_order_consume) :
                              (barbaz || true ? -1 + 0 & -1 : (99 ^ *blep) - 0) &
                              -1 : *blep, memory_order_release)
    | foo : false &&
    !((atomic_fetch_add_explicit(&x, 4 ^ foo, memory_order_acq_rel) &
       (atomic_fetch_xor_explicit(&x, 0, memory_order_acq_rel) |
        atomic_load_explicit(bar, memory_order_relaxed))
       &
       (!barbaz ?
        atomic_fetch_sub_explicit(&x,
                                  atomic_fetch_and_explicit(&y, -1,
                                                            memory_order_seq_cst)
                                  ^ 53, memory_order_acquire)
        : false ? 0 : 0))
      == foo - 4)
    ?
    ((atomic_load_explicit(foobaz, memory_order_acquire) ||
      atomic_load_explicit(bar, memory_order_relaxed) >= 0 ?
      atomic_load_explicit(&y, memory_order_relaxed) | 0 :
      atomic_fetch_sub_explicit(bar, 27 & 0, memory_order_relaxed))
     >=
     (atomic_load_explicit(foobaz, memory_order_acquire) ||
      atomic_load_explicit(bar, memory_order_relaxed) >= 0 ?
      atomic_load_explicit(&y, memory_order_relaxed) | 0 :
      atomic_fetch_sub_explicit(bar, 27 & 0, memory_order_relaxed))
     || false ? 0 & 27 : *blep & atomic_load_explicit(&x, memory_order_seq_cst) ^
     *blep)
    | atomic_fetch_xor_explicit(&x, 0, memory_order_seq_cst) ^ 0 : *blep - *blep
    + 0; |}]

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
    barbaz =
    ((atomic_load_explicit(bar, memory_order_seq_cst) & 0) ==
     (atomic_load_explicit(bar, memory_order_seq_cst) & 0) ? 53 & 0 : foo |
     (((atomic_fetch_xor_explicit(&x, 0, memory_order_release) ==
        atomic_fetch_sub_explicit(&x, 0, memory_order_acquire))
       <
       (atomic_fetch_xor_explicit(&x, 0, memory_order_release) ==
        atomic_fetch_sub_explicit(&x, 0, memory_order_acquire))
       ? atomic_load_explicit(&x, memory_order_seq_cst) ^
       atomic_load_explicit(bar, memory_order_relaxed) |
       atomic_fetch_or_explicit(bar, 0, memory_order_relaxed) ^ foo : 53 -
       atomic_load_explicit(&y, memory_order_consume))
      | atomic_load_explicit(&x, memory_order_relaxed))
     &
     atomic_fetch_xor_explicit(&y, 0 &
                               (barbaz ?
                                atomic_load_explicit(&x, memory_order_seq_cst) :
                                atomic_fetch_or_explicit(&x, 0,
                                                         memory_order_seq_cst)),
                               memory_order_consume))
    == foo;
    barbaz = *blep !=
    ((true ? 0 : 0) < (0 ^ 0) ? 255 :
     atomic_load_explicit(&y, memory_order_acquire));
    barbaz =
    (atomic_load_explicit(&z, memory_order_relaxed) ? !true ? *blep :
     (*blep < 99 ? *blep & 0 | 0 : 0 | 0) <=
     ((true ? 0 : atomic_load_explicit(&x, memory_order_relaxed)) &
      (atomic_fetch_or_explicit(&y, 0, memory_order_acq_rel) & 0) ^ foo)
     ?
     atomic_fetch_and_explicit(&x, 4 < foo ?
                               atomic_load_explicit(bar, memory_order_seq_cst) :
                               -1 | 0, memory_order_acquire)
     : atomic_fetch_or_explicit(&y, 4 - foo, memory_order_acq_rel) :
     atomic_fetch_xor_explicit(&x, false && true ?
                               atomic_load_explicit(&x, memory_order_consume) :
                               foo <= *blep != foo <= *blep ? false && barbaz ?
                               atomic_load_explicit(&x, memory_order_seq_cst) : 0
                               : *blep ^ *blep ^ (*blep ^ *blep),
                               memory_order_acquire))
    >
    ((barbaz && false ?
      (barbaz ? foo & atomic_load_explicit(&x, memory_order_seq_cst) ^ foo &
       atomic_load_explicit(&x, memory_order_seq_cst) :
       (0 != 0 && *blep >= 0 ?
        atomic_fetch_sub_explicit(&x, 0 | 0, memory_order_seq_cst) &
        atomic_fetch_and_explicit(&x, false ? 0 : -1, memory_order_acq_rel) :
        atomic_fetch_or_explicit(bar, -1 & 0, memory_order_consume))
       ^ atomic_load_explicit(bar, memory_order_consume))
      & atomic_load_explicit(&x, memory_order_seq_cst) | *blep : false && false ?
      0 & 53 : 0 & (0 & 99))
     & atomic_fetch_xor_explicit(&x, *blep & 0, memory_order_release));
    barbaz =
    (atomic_fetch_xor_explicit(&y, 99 ^ *blep, memory_order_acq_rel) &
     (atomic_load_explicit(&y, memory_order_consume) &
      atomic_load_explicit(&x, memory_order_consume) |
      (atomic_fetch_sub_explicit(bar, (true ? 0 : 0) ^ 0, memory_order_consume)
       ==
       atomic_fetch_sub_explicit(bar, (true ? 0 : 0) ^ 0, memory_order_consume) ?
       0 & *blep - 99 & (*blep ^ *blep) - (*blep ^ *blep) :
       ((atomic_fetch_xor_explicit(&x, 0 & 95, memory_order_consume) >=
         (true ? 0 : 0) ? 0 & 53 :
         atomic_load_explicit(&y, memory_order_seq_cst))
        <
        atomic_fetch_xor_explicit(bar, true ? 53 ^
                                  atomic_load_explicit(&y, memory_order_acquire)
                                  :
                                  atomic_fetch_sub_explicit(&x, 0 - 0,
                                                            memory_order_consume),
                                  memory_order_relaxed)
        ? (0 & foo) !=
        (atomic_load_explicit(foobaz, memory_order_relaxed) ?
         atomic_fetch_sub_explicit(&x, 0, memory_order_release) &
         atomic_fetch_or_explicit(&y, 0, memory_order_acq_rel) : 99 & 0)
        ? foo : 0 :
        (false ? atomic_fetch_sub_explicit(&x, 0, memory_order_consume) > foo ?
         atomic_fetch_or_explicit(&x,
                                  atomic_load_explicit(&y, memory_order_acquire)
                                  -
                                  atomic_load_explicit(&y, memory_order_acquire),
                                  memory_order_acq_rel)
         : 0 & foo : atomic_load_explicit(bar, memory_order_relaxed) |
         atomic_fetch_or_explicit(&x, true ? 0 : 0, memory_order_seq_cst))
        &
        atomic_fetch_xor_explicit(bar, 0 &
                                  atomic_fetch_or_explicit(&y, 0 & 53,
                                                           memory_order_consume),
                                  memory_order_acq_rel)
        ^
        (!!(0 <= atomic_fetch_or_explicit(&x, 0, memory_order_release) &&
            atomic_load_explicit(&z, memory_order_acquire))
         ? 24768770 : 0 & atomic_load_explicit(bar, memory_order_acquire))
        | atomic_load_explicit(&x, memory_order_relaxed) ^ 27)
       |
       ((barbaz || true &&
         (!atomic_load_explicit(&z, memory_order_relaxed) &&
          (foo ^ atomic_fetch_sub_explicit(&y, 0, memory_order_seq_cst)) >
          (true ? 223728 : 0)))
        &&
        !((atomic_load_explicit(&z, memory_order_relaxed) || false &&
           atomic_load_explicit(&x, memory_order_relaxed) != 1 && 0 <
           atomic_load_explicit(&x, memory_order_relaxed))
          && true)
        ?
        (atomic_fetch_sub_explicit(&x, 0, memory_order_relaxed) >
         atomic_load_explicit(&y, memory_order_consume) &&
         atomic_fetch_add_explicit(&x, 0, memory_order_acq_rel) ==
         atomic_fetch_xor_explicit(&y, 0, memory_order_seq_cst) ?
         atomic_fetch_or_explicit(bar, 0 - 0, memory_order_relaxed) :
         atomic_fetch_sub_explicit(&y, 0, memory_order_relaxed) ^
         atomic_load_explicit(&x, memory_order_relaxed))
        ^
        (atomic_fetch_sub_explicit(&x, 0, memory_order_relaxed) >
         atomic_load_explicit(&y, memory_order_consume) &&
         atomic_fetch_add_explicit(&x, 0, memory_order_acq_rel) ==
         atomic_fetch_xor_explicit(&y, 0, memory_order_seq_cst) ?
         atomic_fetch_or_explicit(bar, 0 - 0, memory_order_relaxed) :
         atomic_fetch_sub_explicit(&y, 0, memory_order_relaxed) ^
         atomic_load_explicit(&x, memory_order_relaxed))
        : 0))))
    >= atomic_fetch_xor_explicit(bar, 4 ^ foo, memory_order_release);
    barbaz =
    (barbaz ? (true ? 0 : 0) == (barbaz ? foo : 0) &&
     ((atomic_fetch_sub_explicit(&y, 0, memory_order_seq_cst) > foo || false) &&
      (atomic_load_explicit(&z, memory_order_seq_cst) ||
       atomic_fetch_add_explicit(&x, 0, memory_order_relaxed) >= 0 && barbaz))
     ?
     atomic_fetch_add_explicit(&x,
                               atomic_fetch_sub_explicit(&y, 0 - 0,
                                                         memory_order_acq_rel)
                               -
                               atomic_fetch_sub_explicit(&y, 0 - 0,
                                                         memory_order_acq_rel),
                               memory_order_relaxed)
     :
     atomic_fetch_xor_explicit(bar,
                               atomic_fetch_add_explicit(&x, 0,
                                                         memory_order_release)
                               <
                               atomic_fetch_add_explicit(&x, 0,
                                                         memory_order_release)
                               ? *blep : 0 ^ 0, memory_order_release)
     | 0 & (atomic_load_explicit(&x, memory_order_consume) ^ 27) : *blep)
    >= (*blep & 0);
    barbaz = atomic_load_explicit(foobaz, memory_order_relaxed) && barbaz;
    barbaz =
    (27 - atomic_load_explicit(&x, memory_order_relaxed) ^
     (true || barbaz ? 99 ^ *blep :
      atomic_load_explicit(bar, memory_order_relaxed) & *blep)
     |
     atomic_fetch_or_explicit(&y, 0 < 0 > (0 < 0) ?
                              atomic_fetch_and_explicit(&x, -1 | 0,
                                                        memory_order_relaxed)
                              : 0 & 0, memory_order_seq_cst))
    ==
    (!(0 <= 0 && atomic_fetch_sub_explicit(&y, 0, memory_order_release) != 0) ?
     *blep ^
     (!barbaz ? atomic_fetch_xor_explicit(&x, 0 & 27, memory_order_acquire) :
      atomic_load_explicit(&x, memory_order_consume))
     : foo - 4 ^ (atomic_load_explicit(bar, memory_order_seq_cst) ^ 95))
    &&
    atomic_fetch_and_explicit(bar,
                              (0 <= 0 ?
                               atomic_load_explicit(&z, memory_order_seq_cst) ||
                               true :
                               atomic_load_explicit(foobaz, memory_order_relaxed)
                               ||
                               atomic_fetch_or_explicit(bar, 0,
                                                        memory_order_consume)
                               >= *blep)
                              ?
                              (true ? true :
                               atomic_load_explicit(foobaz, memory_order_consume))
                              ? true ? -1 & -1 : 27 ^
                              atomic_load_explicit(&x, memory_order_seq_cst) : 95
                              -
                              atomic_fetch_or_explicit(bar, 0,
                                                       memory_order_release)
                              & -562451 :
                              atomic_load_explicit(&y, memory_order_acquire),
                              memory_order_relaxed)
    <=
    ((0 >= 0 ? foo : 0) >=
     (atomic_fetch_xor_explicit(bar, 0, memory_order_consume) ^
      atomic_fetch_add_explicit(&x, 0, memory_order_consume))
     || !true ? false ? atomic_load_explicit(&y, memory_order_acquire) : 0 ^ 0 :
     (0 & 95 ^ foo) &
     (atomic_fetch_xor_explicit(&y, 95 ^
                                atomic_fetch_xor_explicit(bar, 0,
                                                          memory_order_release),
                                memory_order_relaxed)
      | atomic_fetch_or_explicit(&x, 0, memory_order_acq_rel)));
    barbaz = *blep != *blep &&
    (atomic_fetch_sub_explicit(bar, 0 + 0, memory_order_acquire) ^
     atomic_fetch_sub_explicit(bar, 0 + 0, memory_order_acquire))
    >= foo;
    barbaz =
    (atomic_fetch_or_explicit(bar,
                              (!atomic_load_explicit(&z, memory_order_consume) ||
                               (atomic_fetch_or_explicit(bar, 0,
                                                         memory_order_relaxed)
                                <= foo ||
                                atomic_load_explicit(&z, memory_order_consume)))
                              <
                              (!atomic_load_explicit(&z, memory_order_consume) ||
                               (atomic_fetch_or_explicit(bar, 0,
                                                         memory_order_relaxed)
                                <= foo ||
                                atomic_load_explicit(&z, memory_order_consume)))
                              ? atomic_load_explicit(&y, memory_order_relaxed) :
                              0 - 0, memory_order_release)
     ^ atomic_load_explicit(&y, memory_order_relaxed))
    >= *blep &&
    (foo <=
     ((false ? atomic_load_explicit(bar, memory_order_consume) : 0 ^ 0) >
      (0 & 0) ? (0 & (0 ^ 0) | *blep) ^ *blep : *blep == 4 - foo ?
      !(0 >= atomic_fetch_sub_explicit(bar, 0, memory_order_acquire) || barbaz ||
        !(atomic_fetch_sub_explicit(&y, 0, memory_order_acq_rel) <
          atomic_fetch_or_explicit(bar, 0, memory_order_acquire)))
      ? 0 | 0 | 0 & atomic_fetch_and_explicit(bar, -1, memory_order_relaxed) :
      -2147483648 & atomic_fetch_and_explicit(&y, -1, memory_order_seq_cst) &
      (*blep ^ 99) ^ 0 & 95 :
      (atomic_load_explicit(&z, memory_order_acquire) ? foo : *blep) & foo |
      ((95 & 0) >=
       (atomic_load_explicit(foobaz, memory_order_acquire) ? 0 :
        atomic_load_explicit(bar, memory_order_seq_cst))
       ? !true ? 0 & foo : -111 : barbaz ? *blep :
       atomic_fetch_or_explicit(&y, 0, memory_order_consume) ^
       atomic_load_explicit(bar, memory_order_seq_cst)))
     &&
     ((0 & 95 & (0 & -1 ^ 0 & -1)) >
      (atomic_fetch_xor_explicit(&y, 4 - foo, memory_order_acquire) |
       atomic_fetch_add_explicit(&y, 0 &
                                 atomic_fetch_xor_explicit(&y, 0 &
                                                           atomic_fetch_and_explicit
                                                           (&x, false ?
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_acquire)
                                                            : -1,
                                                            memory_order_acq_rel),
                                                           memory_order_release),
                                 memory_order_consume))
      ||
      !((atomic_load_explicit(&x, memory_order_relaxed) ^
         ((0 != *blep && true && atomic_load_explicit(&y, memory_order_seq_cst)
           != -4 ?
           (atomic_fetch_sub_explicit(bar, 0, memory_order_acquire) |
            atomic_load_explicit(bar, memory_order_consume))
           & (atomic_load_explicit(&y, memory_order_consume) ^ 0) : true ? false
           ? *blep : 0 : -16777215 | 0)
          ^
          (atomic_load_explicit(&z, memory_order_seq_cst) < false ?
           atomic_fetch_sub_explicit(bar, 27 -
                                     atomic_load_explicit(&x,
                                                          memory_order_acquire),
                                     memory_order_seq_cst)
           : 0 & 0)))
        >= foo))
     &&
     ((atomic_load_explicit(&z, memory_order_consume) &&
       (foo != (114 | 0) &&
        (!!atomic_load_explicit(foobaz, memory_order_acquire) && !(0 >= 0)))
       ?
       (true && false ?
        (false || atomic_fetch_or_explicit(&y, 0, memory_order_acq_rel) >= 0) &&
        0 == *blep : true <= barbaz)
       ? true && false ? (atomic_load_explicit(&x, memory_order_consume) ^ foo) &
       0 : 95 ^ atomic_load_explicit(bar, memory_order_acquire) : true != barbaz
       ? true ? atomic_fetch_or_explicit(bar, 0 + 0, memory_order_relaxed) :
       atomic_fetch_xor_explicit(bar, false ?
                                 atomic_load_explicit(&y, memory_order_consume) :
                                 atomic_fetch_and_explicit(&x, -1,
                                                           memory_order_seq_cst)
                                 & 0, memory_order_consume)
       : 0 :
       atomic_fetch_and_explicit(&x,
                                 (false ? 0 ^
                                  atomic_fetch_or_explicit(bar, 0,
                                                           memory_order_release)
                                  : -1 + 0)
                                 - 0, memory_order_acquire))
      | foo)
     ==
     ((0 > atomic_load_explicit(bar, memory_order_acquire) && 0 ==
       atomic_load_explicit(bar, memory_order_consume) ? 0 & 0 : 970908564)
      | atomic_fetch_and_explicit(&x, -1 ^ 0, memory_order_acq_rel))
     -
     ((0 > atomic_load_explicit(bar, memory_order_acquire) && 0 ==
       atomic_load_explicit(bar, memory_order_consume) ? 0 & 0 : 970908564)
      | atomic_fetch_and_explicit(&x, -1 ^ 0, memory_order_acq_rel))
     && (0 == *blep && barbaz) && true);
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
    barbaz = atomic_load_explicit(foobaz, memory_order_relaxed) &&
    (foo - foo |
     atomic_fetch_add_explicit(bar,
                               atomic_load_explicit(&x, memory_order_consume) -
                               27, memory_order_acquire)
     ^
     ((atomic_fetch_xor_explicit(&x, 0, memory_order_consume) !=
       atomic_fetch_xor_explicit(&x, 0, memory_order_acquire) ?
       atomic_load_explicit(&y, memory_order_consume) : foo)
      ^ foo & *blep))
    >
    ((barbaz ? 265 <= 0 || barbaz ? 0 &
      atomic_load_explicit(bar, memory_order_seq_cst) :
      atomic_fetch_xor_explicit(&x, 0, memory_order_release) != 0 ? 0 :
      atomic_fetch_or_explicit(&y, 0, memory_order_seq_cst) : foo &
      (atomic_load_explicit(bar, memory_order_relaxed) >= 0 ? foo : foo))
     &
     (*blep &
      (atomic_load_explicit(&x, memory_order_consume) != foo || false ? 0 == -1 ?
       atomic_load_explicit(&x, memory_order_seq_cst) :
       atomic_fetch_xor_explicit(bar, 0, memory_order_seq_cst) : true ? 0 : 0)))
    ||
    (atomic_load_explicit(foobaz, memory_order_consume) || true ||
     (!!true ||
      ((atomic_fetch_sub_explicit(bar,
                                  atomic_fetch_or_explicit(&x, 0,
                                                           memory_order_seq_cst)
                                  & 0, memory_order_acq_rel)
        ^ (0 ^ 2147483647))
       &
       ((0 == 0 ? 0 : foo) |
        (atomic_load_explicit(&z, memory_order_relaxed) ?
         atomic_fetch_sub_explicit(&y, 0, memory_order_seq_cst) :
         atomic_fetch_or_explicit(bar, 0, memory_order_seq_cst))))
      <=
      ((atomic_load_explicit(&y, memory_order_acquire) ==
        atomic_fetch_xor_explicit(bar, 0, memory_order_seq_cst) ?
        atomic_fetch_xor_explicit(bar, 0, memory_order_acquire) ^ foo :
        atomic_load_explicit(bar, memory_order_acquire) ^ 0)
       ^ atomic_load_explicit(&x, memory_order_consume))));
    barbaz = *blep > *blep ||
    atomic_fetch_add_explicit(&y, false ? 0 + 0 : 0 | 0, memory_order_release) >
    (0 >= 0 ? true ? atomic_fetch_xor_explicit(bar, 0, memory_order_relaxed) -
     atomic_fetch_xor_explicit(bar, 0, memory_order_relaxed) :
     atomic_fetch_or_explicit(&y, 0, memory_order_acq_rel) ^ 0 : 27 -
     atomic_load_explicit(&x, memory_order_relaxed) &
     (atomic_load_explicit(bar, memory_order_seq_cst) >
      atomic_fetch_xor_explicit(&x, 0, memory_order_acquire) ?
      atomic_fetch_sub_explicit(bar, 0, memory_order_seq_cst) : 0))
    ||
    !(!barbaz && (0 & atomic_load_explicit(&y, memory_order_seq_cst)) <=
      atomic_fetch_or_explicit(&x, atomic_load_explicit(&x, memory_order_acquire)
                               - 27, memory_order_seq_cst));
    barbaz =
    !((true || atomic_load_explicit(&z, memory_order_relaxed) || false ?
       (0 ^ 0 - 0) & -1 : atomic_load_explicit(&x, memory_order_relaxed))
      !=
      atomic_fetch_sub_explicit(bar, 0 + 0 - 0 != 0 + 0 - 0 ? 4 ^ foo : 0 &
                                atomic_load_explicit(&y, memory_order_seq_cst),
                                memory_order_release));
    barbaz =
    !((-1 &
       (false ? atomic_fetch_and_explicit(&y, -1, memory_order_seq_cst) : 0))
      <= atomic_load_explicit(&y, memory_order_seq_cst));
    barbaz =
    !(((0 | 0) >= atomic_load_explicit(&x, memory_order_acquire) ?
       (*blep | (0 != -16383 ? 0 : 0)) &
       ((1919995593 < *blep ?
         atomic_fetch_xor_explicit(&x, 0, memory_order_acquire) : foo)
        | 0 & atomic_load_explicit(&x, memory_order_consume))
       : true ?
       atomic_fetch_or_explicit(&y, 0 &
                                atomic_fetch_and_explicit(&x, -1,
                                                          memory_order_relaxed),
                                memory_order_consume)
       & (0 & atomic_load_explicit(&x, memory_order_relaxed)) :
       !(atomic_fetch_xor_explicit(&y, 0, memory_order_release) <
         atomic_fetch_and_explicit(bar, -1, memory_order_seq_cst))
       ? atomic_load_explicit(&y, memory_order_relaxed) :
       atomic_fetch_or_explicit(bar, 0 & foo, memory_order_seq_cst))
      <=
      (false ? atomic_load_explicit(bar, memory_order_seq_cst) : 0 |
       atomic_fetch_or_explicit(&y, 0 - 0, memory_order_seq_cst))
      || atomic_load_explicit(&z, memory_order_seq_cst) && false); |}]

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
      atomic_fetch_xor_explicit(&y,
                                (atomic_load_explicit(&x, memory_order_relaxed) >
                                 27 ?
                                 atomic_load_explicit(&z, memory_order_consume) &&
                                 atomic_load_explicit(&z, memory_order_relaxed) :
                                 atomic_load_explicit(&y, memory_order_seq_cst) <
                                 53)
                                ?
                                atomic_fetch_and_explicit(bar,
                                                          (foo >= foo ?
                                                           (barbaz &&
                                                            atomic_load_explicit
                                                            (foobaz,
                                                             memory_order_relaxed))
                                                           <
                                                           (barbaz &&
                                                            atomic_load_explicit
                                                            (foobaz,
                                                             memory_order_relaxed))
                                                           ? barbaz : true || *blep
                                                           > 0 ? !false <= !false :
                                                           (27 ^
                                                            atomic_load_explicit
                                                            (&x,
                                                             memory_order_seq_cst))
                                                           >
                                                           (0 &
                                                            atomic_fetch_or_explicit
                                                            (bar, 0,
                                                             memory_order_release))
                                                           || 0 >=
                                                           atomic_load_explicit
                                                           (&x,
                                                            memory_order_relaxed)
                                                           &&
                                                           (false || 0 >
                                                            atomic_fetch_or_explicit
                                                            (&x, 0,
                                                             memory_order_consume))
                                                           :
                                                           atomic_load_explicit
                                                           (&y,
                                                            memory_order_consume)
                                                           <
                                                           (atomic_fetch_xor_explicit
                                                            (&y, 0,
                                                             memory_order_relaxed)
                                                            ^ 0 - 0))
                                                          ?
                                                          atomic_load_explicit
                                                          (&y,
                                                           memory_order_consume)
                                                          >= 53 ?
                                                          atomic_fetch_xor_explicit
                                                          (bar, 95 -
                                                           atomic_fetch_add_explicit
                                                           (bar, 0 &
                                                            atomic_load_explicit
                                                            (bar,
                                                             memory_order_relaxed),
                                                            memory_order_seq_cst),
                                                           memory_order_acquire)
                                                          != 95 ?
                                                          atomic_fetch_xor_explicit
                                                          (&y, 53 & 0,
                                                           memory_order_relaxed)
                                                          : -1 ^ 0 : true ?
                                                          atomic_fetch_sub_explicit
                                                          (bar,
                                                           (atomic_fetch_or_explicit
                                                            (bar, 0,
                                                             memory_order_seq_cst)
                                                            >= 95 ? true :
                                                            !(0 >= foo))
                                                           ?
                                                           (false ?
                                                            atomic_fetch_add_explicit
                                                            (bar, 0,
                                                             memory_order_acquire)
                                                            : 0)
                                                           ^
                                                           (false ?
                                                            atomic_fetch_add_explicit
                                                            (bar, 0,
                                                             memory_order_acquire)
                                                            : 0)
                                                           :
                                                           (false ?
                                                            atomic_fetch_sub_explicit
                                                            (bar, 0,
                                                             memory_order_acq_rel)
                                                            == 0 ? 0 :
                                                            atomic_fetch_add_explicit
                                                            (&y, 0,
                                                             memory_order_acq_rel)
                                                            : 0 | 0)
                                                           | 27 -
                                                           atomic_load_explicit
                                                           (&x,
                                                            memory_order_consume),
                                                           memory_order_consume)
                                                          & *blep : (false ? 0 : 0)
                                                          != (false ? 0 : 0) ? 0 +
                                                          0 | 0 & 0 :
                                                          atomic_fetch_xor_explicit
                                                          (&x, 0 +
                                                           (atomic_fetch_sub_explicit
                                                            (bar, 0,
                                                             memory_order_seq_cst)
                                                            ^
                                                            atomic_fetch_sub_explicit
                                                            (bar, 0,
                                                             memory_order_seq_cst)),
                                                           memory_order_relaxed)
                                                          ^ 27 :
                                                          atomic_fetch_xor_explicit
                                                          (bar, false <
                                                           atomic_load_explicit
                                                           (&z,
                                                            memory_order_relaxed)
                                                           ?
                                                           atomic_fetch_or_explicit
                                                           (&y, 0 - 0,
                                                            memory_order_seq_cst)
                                                           : 0 + 0,
                                                           memory_order_acquire)
                                                          ^
                                                          atomic_fetch_xor_explicit
                                                          (bar, false <
                                                           atomic_load_explicit
                                                           (&z,
                                                            memory_order_relaxed)
                                                           ?
                                                           atomic_fetch_or_explicit
                                                           (&y, 0 - 0,
                                                            memory_order_seq_cst)
                                                           : 0 + 0,
                                                           memory_order_acquire),
                                                          memory_order_relaxed)
                                : 0 & *blep, memory_order_consume)
      ==
      ((atomic_fetch_or_explicit(bar, 95 ==
                                 atomic_fetch_add_explicit(bar, 0,
                                                           memory_order_seq_cst)
                                 ? 0 - 0 : 27 & 0, memory_order_seq_cst)
        ^
        atomic_fetch_or_explicit(bar, 95 ==
                                 atomic_fetch_add_explicit(bar, 0,
                                                           memory_order_seq_cst)
                                 ? 0 - 0 : 27 & 0, memory_order_seq_cst))
       &
       (((!(atomic_fetch_sub_explicit(bar, 0, memory_order_consume) == *blep) <
          !(atomic_fetch_sub_explicit(bar, 0, memory_order_consume) == *blep) ?
          atomic_load_explicit(&x, memory_order_seq_cst) ^
          atomic_fetch_xor_explicit(&x, (false ? 0 >= *blep : true) ? *blep ^ 99 :
                                    atomic_fetch_and_explicit(bar, -1 & -1,
                                                              memory_order_relaxed),
                                    memory_order_relaxed)
          |
          (barbaz ? 0 >= atomic_load_explicit(&y, memory_order_consume) &&
           atomic_load_explicit(&z, memory_order_seq_cst) ||
           atomic_fetch_or_explicit(&x, 0, memory_order_consume) >
           atomic_fetch_add_explicit(bar, 0, memory_order_relaxed) ? true ? 0 | 0 :
           0 & 0 : atomic_load_explicit(foobaz, memory_order_seq_cst) ?
           atomic_load_explicit(bar, memory_order_seq_cst) :
           atomic_load_explicit(&y, memory_order_seq_cst) :
           atomic_fetch_add_explicit(&x,
                                     (atomic_fetch_add_explicit(&x, 0,
                                                                memory_order_relaxed)
                                      &
                                      atomic_fetch_xor_explicit(&x, 0,
                                                                memory_order_acq_rel))
                                     -
                                     (atomic_fetch_add_explicit(&x, 0,
                                                                memory_order_relaxed)
                                      &
                                      atomic_fetch_xor_explicit(&x, 0,
                                                                memory_order_acq_rel)),
                                     memory_order_release))
          : atomic_load_explicit(&y, memory_order_consume) ^ 53)
         | foo)
        &
        (false ? 27 - atomic_load_explicit(&x, memory_order_relaxed) :
         atomic_fetch_xor_explicit(&x,
                                   (foo |
                                    atomic_fetch_or_explicit(&y, 0,
                                                             memory_order_seq_cst))
                                   !=
                                   (foo |
                                    atomic_fetch_or_explicit(&y, 0,
                                                             memory_order_seq_cst))
                                   ? true ? 0 :
                                   atomic_load_explicit(&x, memory_order_consume) :
                                   0, memory_order_release))));
      barbaz = 0 !=
      ((atomic_load_explicit(&z, memory_order_seq_cst) ? 0 + 0 :
        atomic_fetch_or_explicit(bar, 0, memory_order_consume) |
        atomic_load_explicit(bar, memory_order_relaxed))
       ^
       (barbaz ? atomic_load_explicit(foobaz, memory_order_acquire) ? 0 : 0 :
        atomic_load_explicit(&x, memory_order_relaxed)));
      barbaz = *blep !=
      ((true ? 0 : 0) < (0 ^ 0) ? 255 :
       atomic_load_explicit(&y, memory_order_acquire));
      barbaz = (0 & -1) !=
      (99 != *blep ? *blep : 0 &
       (atomic_fetch_and_explicit(&x, false ?
                                  atomic_load_explicit(bar, memory_order_seq_cst) |
                                  foo : -1, memory_order_release)
        |
        (atomic_load_explicit(bar, memory_order_relaxed) > 0 || true ? 0 |
         atomic_fetch_and_explicit(&x, -1, memory_order_consume) :
         atomic_fetch_sub_explicit(&x, 53 ^
                                   atomic_fetch_sub_explicit(&y, 0,
                                                             memory_order_release),
                                   memory_order_release))));
      barbaz = false && atomic_load_explicit(&z, memory_order_seq_cst);
      barbaz = atomic_load_explicit(foobaz, memory_order_consume) &&
      (barbaz || atomic_load_explicit(&x, memory_order_seq_cst) <=
       ((foo <
         (0 >= atomic_load_explicit(bar, memory_order_relaxed) && true ? true ? foo
          : 0 : atomic_load_explicit(&y, memory_order_relaxed) | 0)
         ||
         (atomic_load_explicit(foobaz, memory_order_relaxed) && (0 | 0) <=
          (0 & atomic_fetch_and_explicit(&x, -1, memory_order_acq_rel)) || barbaz)
         ? atomic_load_explicit(&y, memory_order_relaxed) :
         atomic_fetch_or_explicit(&y, 99 - *blep, memory_order_relaxed) ^
         (atomic_load_explicit(&y, memory_order_consume) >= foo || foo >= *blep ?
          foo :
          !(atomic_fetch_xor_explicit(bar, 0, memory_order_acq_rel) ==
            atomic_fetch_xor_explicit(bar, 0, memory_order_seq_cst))
          ? 0 ^ atomic_load_explicit(bar, memory_order_seq_cst) :
          atomic_fetch_or_explicit(bar, 0, memory_order_acquire) & foo)
         ^ atomic_load_explicit(&y, memory_order_relaxed))
        |
        (true != barbaz ?
         !((barbaz || atomic_load_explicit(&z, memory_order_consume)) && foo <=
           2249)
         ||
         !((false || *blep > atomic_load_explicit(&y, memory_order_consume)) &&
           (0 > 1035516393 || 0 != 0))
         ?
         atomic_fetch_add_explicit(&y,
                                   atomic_load_explicit(&y, memory_order_relaxed) ^
                                   atomic_load_explicit(&y, memory_order_relaxed),
                                   memory_order_seq_cst)
         ^
         (atomic_load_explicit(&x, memory_order_relaxed) |
          (true ? atomic_fetch_or_explicit(&x, -1 & 0, memory_order_acq_rel) :
           atomic_load_explicit(&y, memory_order_acquire)))
         : atomic_load_explicit(&x, memory_order_relaxed) :
         (atomic_load_explicit(bar, memory_order_relaxed) ^ 0) ==
         (atomic_load_explicit(bar, memory_order_relaxed) ^ 0) ?
         atomic_load_explicit(bar, memory_order_seq_cst) - 95 :
         ((atomic_fetch_and_explicit(&y, -1, memory_order_release) >= *blep ?
           atomic_fetch_and_explicit(&x, -1, memory_order_acquire) :
           atomic_load_explicit(bar, memory_order_seq_cst))
          > foo ? 0 : foo & foo ^
          (atomic_load_explicit(bar, memory_order_acquire) ^ 0))
         & atomic_load_explicit(&x, memory_order_consume))));
      barbaz = *blep != *blep &&
      (atomic_fetch_sub_explicit(bar, 0 + 0, memory_order_acquire) ^
       atomic_fetch_sub_explicit(bar, 0 + 0, memory_order_acquire))
      >= foo;
      barbaz = (atomic_load_explicit(&y, memory_order_acquire) ^ foo) >= (-1 & 0)
      && true;
      barbaz = !barbaz;
      foo--;
      foo++;
      foo = 0;
      foo =
      atomic_fetch_add_explicit(bar, 27 -
                                atomic_load_explicit(&x, memory_order_consume),
                                memory_order_release)
      & atomic_fetch_sub_explicit(&y, 4 ^ foo, memory_order_relaxed);
      foo = atomic_fetch_xor_explicit(&x, 0, memory_order_acquire) ^ 0;
      (*blep)--;
      *blep = foo; |}]

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
