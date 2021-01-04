(* This file is part of c4f. Copyright (c) 2018--2020 Matt Windsor and
   contributors - c4t itself is licensed under the MIT License. See the
   LICENSE file in the project root for more information. - ACT is based in
   part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Q = Base_quickcheck
  module Src = C4f_fuzz
end

let%test_module "pp" =
  ( module struct
    let test (flag : Src.Flag.t) : unit = Fmt.pr "@[%a@]@." Src.Flag.pp flag

    let%expect_test "normal flag" =
      test (Or_error.ok_exn (Src.Flag.try_make ~wins:2 ~losses:1)) ;
      [%expect {| 2:1 odds on |}]

    let%expect_test "always-true flag" =
      test (Src.Flag.exact true) ;
      [%expect {| true |}]

    let%expect_test "always-false flag" =
      test (Src.Flag.exact false) ;
      [%expect {| false |}]
  end )

let%test_module "to_exact_opt" =
  ( module struct
    let test (flag : Src.Flag.t) : unit =
      Fmt.(pr "@[%a@]@." (using Src.Flag.to_exact_opt (option Bool.pp))) flag

    let%expect_test "normal flag" =
      test (Or_error.ok_exn (Src.Flag.try_make ~wins:2 ~losses:1)) ;
      [%expect]

    let%expect_test "always-true flag" =
      test (Src.Flag.exact true) ;
      [%expect {| true |}]

    let%expect_test "always-false flag" =
      test (Src.Flag.exact false) ;
      [%expect {| false |}]
  end )

module Flag_gen (B : sig
  val wins_generator : int Q.Generator.t

  val losses_generator : int Q.Generator.t
end) =
struct
  type t = Src.Flag.t [@@deriving sexp_of]

  let quickcheck_generator =
    Q.Generator.Let_syntax.(
      let%map wins = B.wins_generator and losses = B.losses_generator in
      let f = Src.Flag.try_make ~wins ~losses in
      Or_error.ok_exn f)

  let quickcheck_shrinker = Q.Shrinker.atomic
end

module Gen_true_flag = Flag_gen (struct
  let wins_generator = Q.Generator.small_strictly_positive_int

  let losses_generator = Q.Generator.return 0
end)

module Gen_false_flag = Flag_gen (struct
  let wins_generator = Q.Generator.return 0

  let losses_generator = Q.Generator.small_strictly_positive_int
end)

let%test_module "exact-flag recognition" =
  ( module struct
    let%test_unit "flags with positive wins and 0 losses are always \
                   exact-true" =
      Q.Test.run_exn
        (module Gen_true_flag)
        ~f:(fun f ->
          [%test_result: bool option] ~here:[[%here]]
            ~equal:[%equal: bool option] ~expect:(Some true)
            (Src.Flag.to_exact_opt f))

    let%test_unit "flags with positive losses and 0 wins are always \
                   exact-false" =
      Q.Test.run_exn
        (module Gen_false_flag)
        ~f:(fun f ->
          [%test_result: bool option] ~here:[[%here]]
            ~equal:[%equal: bool option] ~expect:(Some false)
            (Src.Flag.to_exact_opt f))
  end )

module To_bool_generator (G : sig
  val flag : Src.Flag.t
end) =
struct
  type t = bool [@@deriving sexp]

  let quickcheck_generator = Src.Flag.as_quickcheck_generator G.flag

  let quickcheck_shrinker = Q.Shrinker.atomic
end

let%test_module "use as quickcheck_generators" =
  ( module struct
    let%test_unit "always-true flags always generate true" =
      Q.Test.run_exn
        ( module To_bool_generator (struct
          let flag = Src.Flag.exact true
        end) )
        ~f:
          ([%test_result: bool] ~here:[[%here]] ~equal:[%equal: bool]
             ~expect:true)

    let%test_unit "always-false flags always generate false" =
      Q.Test.run_exn
        ( module To_bool_generator (struct
          let flag = Src.Flag.exact false
        end) )
        ~f:
          ([%test_result: bool] ~here:[[%here]] ~equal:[%equal: bool]
             ~expect:false)
  end )
