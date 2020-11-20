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
    atomic_fetch_and_explicit(bar, false ?
                              atomic_load_explicit(&y, memory_order_acquire) :
                              -1, memory_order_acq_rel)
    ^ foo - 4;
    (*blep)--;
    (*blep)++;
    *blep = barbaz ? 0 > 0 ? true ? 0 : 0 : 0 & foo :
    atomic_load_explicit(bar, memory_order_consume);
    *blep = atomic_load_explicit(&z, memory_order_seq_cst) ? (false ? *blep : 0)
    ^ (0 ^ 0) : atomic_load_explicit(&x, memory_order_relaxed); |}]

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
    barbaz = atomic_load_explicit(foobaz, memory_order_consume);
    barbaz = *blep ==
    (true ? atomic_load_explicit(bar, memory_order_acquire) :
     atomic_fetch_add_explicit(bar, 0, memory_order_consume));
    barbaz =
    atomic_fetch_xor_explicit(&y,
                              atomic_fetch_and_explicit(bar, false ? *blep |
                                                        atomic_load_explicit
                                                        (&x,
                                                         memory_order_relaxed)
                                                        : true ? -1 :
                                                        atomic_fetch_add_explicit
                                                        (bar, 0,
                                                         memory_order_seq_cst),
                                                        memory_order_seq_cst)
                              ^
                              atomic_fetch_and_explicit(bar, false ? *blep |
                                                        atomic_load_explicit
                                                        (&x,
                                                         memory_order_relaxed)
                                                        : true ? -1 :
                                                        atomic_fetch_add_explicit
                                                        (bar, 0,
                                                         memory_order_seq_cst),
                                                        memory_order_seq_cst),
                              memory_order_acquire)
    ==
    (*blep |
     (!true ? atomic_fetch_or_explicit(&y, false ? 0 : 0, memory_order_relaxed) :
      (true ? 0 : atomic_load_explicit(&x, memory_order_seq_cst)) ^ 0 & 0));
    barbaz = atomic_load_explicit(&x, memory_order_acquire) !=
    (atomic_fetch_add_explicit(&x, false ? 0 : 0, memory_order_seq_cst) == 27 ?
     (false ? *blep : 0) & atomic_load_explicit(&y, memory_order_consume) : false
     ? 0 : 0);
    barbaz = (true ? 0 : 0) >
    (atomic_load_explicit(&y, memory_order_seq_cst) & *blep);
    barbaz = atomic_load_explicit(foobaz, memory_order_consume) &&
    (barbaz || atomic_load_explicit(&x, memory_order_seq_cst) >=
     ((false || 0 > atomic_load_explicit(bar, memory_order_seq_cst) ? -18166770 :
       true ? foo : 0)
      |
      ((0 | 0) & atomic_load_explicit(&y, memory_order_consume) -
       atomic_load_explicit(&y, memory_order_consume) ^
       (0 < 0 ? atomic_load_explicit(&y, memory_order_relaxed) : 0) |
       atomic_load_explicit(&y, memory_order_relaxed) |
       atomic_fetch_or_explicit(&y, 99 - *blep, memory_order_relaxed) ^
       (0 == atomic_fetch_sub_explicit(&x, 0, memory_order_acq_rel) ? 0 : 0) ^
       atomic_load_explicit(&y, memory_order_consume))));
    barbaz =
    ((0 <= atomic_fetch_add_explicit(&y, 0, memory_order_acquire) || false) &&
     (atomic_fetch_add_explicit(&x, 0, memory_order_relaxed) <= 0 &&
      atomic_load_explicit(bar, memory_order_relaxed) == 0)
     || atomic_load_explicit(foobaz, memory_order_relaxed))
    &&
    (!barbaz || !(atomic_fetch_xor_explicit(&x, 0, memory_order_relaxed) > 0));
    barbaz =
    (barbaz &&
     ((atomic_load_explicit(&y, memory_order_acquire) ==
       atomic_fetch_xor_explicit(bar, 0, memory_order_seq_cst) ?
       atomic_fetch_xor_explicit(bar, 0, memory_order_acquire) ^ foo :
       atomic_load_explicit(bar, memory_order_acquire) ^ 0)
      ^ atomic_load_explicit(&x, memory_order_consume) |
      (!true ? atomic_fetch_and_explicit(&y, -1, memory_order_seq_cst) | 0 :
       atomic_load_explicit(&x, memory_order_consume) | 0)
      ^
      (0 | atomic_load_explicit(&x, memory_order_relaxed) |
       atomic_fetch_sub_explicit(&x, true ? 0 : 0, memory_order_relaxed)))
     > (atomic_load_explicit(&y, memory_order_consume) ^ 53) || *blep <
     (true ? 0 + 0 :
      atomic_fetch_or_explicit(&x, true ? 0 :
                               atomic_fetch_add_explicit(&x, 0,
                                                         memory_order_release),
                               memory_order_seq_cst))
     ||
     (false || atomic_load_explicit(&y, memory_order_acquire) -
      atomic_load_explicit(&y, memory_order_acquire) >=
      (false && barbaz ? foo >= foo ?
       atomic_fetch_xor_explicit(&x, 0, memory_order_release) :
       atomic_fetch_sub_explicit(&x, 0, memory_order_consume) :
       atomic_fetch_or_explicit(&x, 0, memory_order_relaxed) | *blep))
     && atomic_load_explicit(&z, memory_order_relaxed))
    && atomic_load_explicit(&z, memory_order_consume);
    barbaz = 0 > atomic_load_explicit(bar, memory_order_consume) ||
    atomic_load_explicit(&y, memory_order_acquire) !=
    atomic_fetch_or_explicit(&x, 0, memory_order_acquire);
    barbaz = false ||
    !((atomic_fetch_and_explicit(&x, 0 ^
                                 (false ?
                                  atomic_load_explicit(bar, memory_order_consume)
                                  : -1),
                                 memory_order_acq_rel)
       & *blep)
      >=
      (*blep ^
       (foo |
        (atomic_fetch_sub_explicit(bar, 0, memory_order_acquire) >= 0 ? foo :
         foo))))
    || false;
    barbaz = *blep > *blep ||
    atomic_fetch_add_explicit(&y, false ? false ?
                              atomic_load_explicit(&x, memory_order_relaxed) : 0
                              : 0 & -29, memory_order_release)
    == atomic_load_explicit(bar, memory_order_seq_cst) || barbaz;
    barbaz =
    !((false ||
       (atomic_fetch_xor_explicit(&y, (true ? 0 : 0) & 4, memory_order_release) >
        atomic_fetch_add_explicit(bar, *blep & 0 &
                                  (atomic_fetch_sub_explicit(&y, 0,
                                                             memory_order_acquire)
                                   ^
                                   atomic_fetch_sub_explicit(&y, 0,
                                                             memory_order_acquire)),
                                  memory_order_acq_rel)
        ||
        (atomic_load_explicit(&x, memory_order_relaxed) |
         (true ? atomic_load_explicit(bar, memory_order_seq_cst) : *blep))
        > atomic_load_explicit(&y, memory_order_acquire)))
      && atomic_load_explicit(foobaz, memory_order_relaxed))
    || atomic_load_explicit(foobaz, memory_order_seq_cst) && false; |}]

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
      barbaz = barbaz;
      barbaz = atomic_load_explicit(foobaz, memory_order_consume) &&
      (barbaz || atomic_load_explicit(&x, memory_order_seq_cst) >=
       ((false || 0 > atomic_load_explicit(bar, memory_order_seq_cst) ? -18166770 :
         true ? foo : 0)
        |
        ((0 | 0) & atomic_load_explicit(&y, memory_order_consume) -
         atomic_load_explicit(&y, memory_order_consume) ^
         (0 < 0 ? atomic_load_explicit(&y, memory_order_relaxed) : 0) |
         atomic_load_explicit(&y, memory_order_relaxed) |
         atomic_fetch_or_explicit(&y, 99 - *blep, memory_order_relaxed) ^
         (0 == atomic_fetch_sub_explicit(&x, 0, memory_order_acq_rel) ? 0 : 0) ^
         atomic_load_explicit(&y, memory_order_consume))));
      barbaz = (atomic_load_explicit(&y, memory_order_acquire) ^ foo) != (-1 & 0)
      &&
      (atomic_load_explicit(&z, memory_order_consume) ?
       atomic_fetch_xor_explicit(&y, 0, memory_order_acq_rel) :
       atomic_fetch_add_explicit(&x, 0, memory_order_relaxed))
      > 0 + 0;
      barbaz = barbaz ||
      !((99 & 0 | atomic_load_explicit(&x, memory_order_acquire) - 27) <
        atomic_fetch_sub_explicit(&y, *blep ^ 99, memory_order_acq_rel))
      || true;
      barbaz = *blep > *blep ||
      atomic_fetch_add_explicit(&y, false ? false ?
                                atomic_load_explicit(&x, memory_order_relaxed) : 0
                                : 0 & -29, memory_order_release)
      == atomic_load_explicit(bar, memory_order_seq_cst) || barbaz;
      barbaz = !barbaz ||
      !((atomic_load_explicit(&z, memory_order_relaxed) ? 0 : 0) >
        (0 & (0 & atomic_fetch_add_explicit(&x, 0, memory_order_seq_cst))));
      barbaz =
      !((barbaz ||
         atomic_fetch_or_explicit(&x,
                                  (false ?
                                   atomic_fetch_or_explicit(bar, 0,
                                                            memory_order_acq_rel)
                                   : 0)
                                  & *blep & foo, memory_order_seq_cst)
         < *blep)
        &&
        ((atomic_fetch_xor_explicit(&y,
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
           : atomic_fetch_sub_explicit(&y, 0, memory_order_consume) <=
           atomic_fetch_and_explicit(&x, -1, memory_order_release) ? 0 :
           atomic_fetch_or_explicit(&x, 0, memory_order_seq_cst)))
         <= *blep || false));
      foo--;
      foo++;
      foo = 0;
      foo =
      atomic_fetch_and_explicit(&x,
                                (atomic_load_explicit(bar, memory_order_seq_cst) -
                                 95 &
                                 (atomic_fetch_or_explicit(&y, 0,
                                                           memory_order_seq_cst)
                                  &
                                  atomic_load_explicit(bar, memory_order_seq_cst)))
                                <
                                (atomic_load_explicit(bar, memory_order_seq_cst) -
                                 95 &
                                 (atomic_fetch_or_explicit(&y, 0,
                                                           memory_order_seq_cst)
                                  &
                                  atomic_load_explicit(bar, memory_order_seq_cst)))
                                ?
                                (atomic_load_explicit(&y, memory_order_acquire) ==
                                 atomic_fetch_xor_explicit(bar, 0,
                                                           memory_order_seq_cst)
                                 ?
                                 atomic_fetch_xor_explicit(bar, 0,
                                                           memory_order_acquire)
                                 ^ foo :
                                 atomic_load_explicit(bar, memory_order_acquire) ^
                                 0)
                                ^ atomic_load_explicit(&x, memory_order_consume) |
                                (!true ?
                                 atomic_fetch_and_explicit(&y, -1,
                                                           memory_order_seq_cst)
                                 | 0 :
                                 atomic_load_explicit(&x, memory_order_consume) |
                                 0)
                                ^
                                (0 | atomic_load_explicit(&x, memory_order_relaxed)
                                 |
                                 atomic_fetch_sub_explicit(&x, true ? 0 : 0,
                                                           memory_order_relaxed))
                                : -1, memory_order_acq_rel);
      foo = atomic_load_explicit(&y, memory_order_seq_cst) &
      ((0 ^ atomic_load_explicit(bar, memory_order_seq_cst)) !=
       (0 ^ atomic_load_explicit(bar, memory_order_seq_cst)) ? *blep : 0 & 0 & 4);
      foo = atomic_fetch_xor_explicit(&x, 0, memory_order_acquire) ^ 0;
      (*blep)--;
      (*blep)++; |}]

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
