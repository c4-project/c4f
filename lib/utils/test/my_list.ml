(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
open Base_quickcheck
open Act_utils.My_list

let%test_module "find_one_opt" =
  ( module struct
    let f (x : int) : string option =
      Option.some_if (Int.is_pow2 x) (Int.to_string x)

    let p (s : string option Or_error.t) : unit =
      Stdio.print_s [%sexp (s : string option Or_error.t)]

    let%expect_test "find_one_opt: none" =
      p (find_one_opt ~f [3; 5; 11; 94]) ;
      [%expect {| (Ok ()) |}]

    let%expect_test "find_one_opt: one" =
      p (find_one_opt ~f [3; 4; 5; 11; 94]) ;
      [%expect {| (Ok (4)) |}]

    let%expect_test "find_one_opt: multiple" =
      p (find_one_opt ~f [3; 4; 5; 11; 64; 94]) ;
      [%expect {| (Error "Duplicate item") |}]
  end )

module Int_list = struct
  type t = int list [@@deriving sexp, quickcheck]
end

let%test_module "Random" =
  ( module struct
    let make_random () : Splittable_random.State.t =
      let rng = Random.State.make_self_init ~allow_in_tests:true () in
      Splittable_random.State.create rng

    let check_random (xs : int list) ~(pred : int list -> 'a -> bool)
        ~(f : int list -> 'a option) : bool =
      xs |> f |> Option.value_map ~default:(List.is_empty xs) ~f:(pred xs)

    let in_bounds (type a) (xs : a list) (i : int) : bool =
      Option.is_some (List.nth xs i)

    let%test_unit "random_index is always in bounds" =
      let random = make_random () in
      Test.run_exn
        (module Int_list)
        ~f:
          ([%test_pred: int list] ~here:[[%here]]
             (check_random ~f:(random_index ~random) ~pred:in_bounds))

    let%test_unit "random_stride is always in bounds" =
      let random = make_random () in
      Test.run_exn
        (module Int_list)
        ~f:
          ([%test_pred: int list] ~here:[[%here]]
             (check_random ~f:(random_stride ~random)
                ~pred:(fun xs (i, n) ->
                  in_bounds xs i && n <= List.length (List.drop xs (i - 1)))))

    let%test_module "random_item" =
      ( module struct
        let%expect_test "random item: empty list" =
          let deterministic_srng = Splittable_random.State.of_int 0 in
          print_s
            [%sexp (random_item ~random:deterministic_srng [] : int option)] ;
          [%expect {| () |}]

        let%test_unit "random_item is always a valid item" =
          let rng = Random.State.make_self_init ~allow_in_tests:true () in
          let random = Splittable_random.State.create rng in
          Test.run_exn
            (module Int_list)
            ~f:
              ([%test_pred: int list] ~here:[[%here]]
                 (check_random ~f:(random_item ~random)
                    ~pred:(List.mem ~equal:Int.equal)))
      end )
  end )

let%test_module "splice" =
  ( module struct
    let test : int list Or_error.t -> unit =
      Fmt.(pr "@[%a@]@." (result ~error:Error.pp ~ok:(list ~sep:comma int)))

    let test_fib ~(start : int) ~(length : int)
        ~(replace_f : int list -> int list) : unit =
      test (splice [1; 1; 2; 3; 5; 8; 13; 21] ~start ~length ~replace_f)

    let%test_module "in-bounds" =
      ( module struct
        let test_in_bounds : replace_f:(int list -> int list) -> unit =
          test_fib ~start:2 ~length:3

        let%expect_test "in-bounds, do nothing" =
          test_in_bounds ~replace_f:Fn.id ;
          [%expect {| 1, 1, 2, 3, 5, 8, 13, 21 |}]

        let%expect_test "in-bounds, drop items" =
          test_in_bounds ~replace_f:(Fn.const []) ;
          [%expect {| 1, 1, 8, 13, 21 |}]

        let%expect_test "in-bounds, reverse items" =
          test_in_bounds ~replace_f:List.rev ;
          [%expect {| 1, 1, 5, 3, 2, 8, 13, 21 |}]
      end )

    let%test_module "out-of-bounds" =
      ( module struct
        let%expect_test "out of bounds (negative start)" =
          test_fib ~start:(-1) ~length:3 ~replace_f:Fn.id ;
          [%expect {| Can't split a list at a negative point -1 |}]

        let%expect_test "out of bounds (over-far start)" =
          test_fib ~start:8 ~length:3 ~replace_f:Fn.id ;
          [%expect {| Can't split a list of length 0 at point 3 |}]

        let%expect_test "out of bounds (negative length)" =
          test_fib ~start:2 ~length:(-3) ~replace_f:Fn.id ;
          [%expect {| Can't split a list at a negative point -3 |}]

        let%expect_test "out of bounds (overlong length)" =
          test_fib ~start:2 ~length:7 ~replace_f:Fn.id ;
          [%expect {| Can't split a list of length 6 at point 7 |}]
      end )

    let%test_module "pathological cases" =
      ( module struct
        let%expect_test "zero-length list, reverse items" =
          test_fib ~start:2 ~length:0 ~replace_f:List.rev ;
          [%expect {| 1, 1, 2, 3, 5, 8, 13, 21 |}]

        let%test_unit "splice on a full list behaves as operating directly"
            =
          Test.run_exn
            ( module struct
              type t = int * int list * (int list -> int list)
              [@@deriving sexp, quickcheck]
            end )
            ~f:(fun (x, xs, f) ->
              [%test_result: int list Or_error.t] ~here:[[%here]]
                ~equal:[%compare.equal: int list Or_error.t]
                ~expect:(Or_error.return (f (x :: xs)))
                (splice (x :: xs) ~start:0
                   ~length:(List.length (x :: xs))
                   ~replace_f:f))
      end )
  end )
