(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
open Base_quickcheck
open C4f_utils.My_list

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
      let rng = Base.Random.State.make_self_init ~allow_in_tests:true () in
      Splittable_random.State.create rng

    let check_random (xs : int list) ~(pred : int list -> 'a -> bool)
        ~(f : int list -> 'a option) : bool =
      xs |> f |> Option.value_map ~default:(List.is_empty xs) ~f:(pred xs)

    let in_bounds (type a) (xs : a list) (i : int) : bool =
      Option.is_some (List.nth xs i)

    let%test_unit "index is always in bounds" =
      let random = make_random () in
      Test.run_exn
        (module Int_list)
        ~f:
          ([%test_pred: int list]
             ~here:[[%here]]
             (check_random ~f:(Random.index ~random) ~pred:in_bounds) )

    let%test_unit "span is always in bounds" =
      let random = make_random () in
      Test.run_exn
        (module Int_list)
        ~f:
          ([%test_pred: int list]
             ~here:[[%here]]
             (check_random ~f:(Random.span ~random)
                ~pred:(fun xs {Span.pos; len} ->
                  in_bounds xs pos
                  && len <= List.length (List.drop xs (pos - 1)) ) ) )

    let%test_module "item" =
      ( module struct
        let%expect_test "empty list" =
          let deterministic_srng = Splittable_random.State.of_int 0 in
          print_s
            [%sexp (Random.item ~random:deterministic_srng [] : int option)] ;
          [%expect {| () |}]

        let%test_unit "always a valid item" =
          let random = make_random () in
          Test.run_exn
            (module Int_list)
            ~f:
              ([%test_pred: int list]
                 ~here:[[%here]]
                 (check_random ~f:(Random.item ~random)
                    ~pred:(List.mem ~equal:Int.equal) ) )
      end )
  end )

let test_splice_like : int list Or_error.t -> unit =
  Fmt.(pr "@[%a@]@." (result ~error:Error.pp ~ok:(list ~sep:comma int)))

let fibs : int list = [1; 1; 2; 3; 5; 8; 13; 21]

let%test_module "splice" =
  ( module struct
    let test_fib ~(pos : int) ~(len : int) ~(replace_f : int list -> int list)
        : unit =
      test_splice_like (splice fibs ~span:{pos; len} ~replace_f)

    let%test_module "in-bounds" =
      ( module struct
        let test_in_bounds : replace_f:(int list -> int list) -> unit =
          test_fib ~pos:2 ~len:3

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
        let%expect_test "out of bounds (negative pos)" =
          test_fib ~pos:(-1) ~len:3 ~replace_f:Fn.id ;
          [%expect {| Can't split a list at a negative point -1 |}]

        let%expect_test "out of bounds (over-far pos)" =
          test_fib ~pos:8 ~len:3 ~replace_f:Fn.id ;
          [%expect {| Can't split a list of length 0 at point 3 |}]

        let%expect_test "out of bounds (negative len)" =
          test_fib ~pos:2 ~len:(-3) ~replace_f:Fn.id ;
          [%expect {| Can't split a list at a negative point -3 |}]

        let%expect_test "out of bounds (overlong len)" =
          test_fib ~pos:2 ~len:7 ~replace_f:Fn.id ;
          [%expect {| Can't split a list of length 6 at point 7 |}]
      end )

    let%test_module "pathological cases" =
      ( module struct
        let%expect_test "zero-len list, reverse items" =
          test_fib ~pos:2 ~len:0 ~replace_f:List.rev ;
          [%expect {| 1, 1, 2, 3, 5, 8, 13, 21 |}]

        let%test_unit "splice on a full list behaves as operating directly" =
          Test.run_exn
            ( module struct
              type t = int list * (int list -> int list)
              [@@deriving sexp, quickcheck]
            end )
            ~f:(fun (xs, f) ->
              [%test_result: int list Or_error.t]
                ~here:[[%here]]
                ~equal:[%compare.equal: int list Or_error.t]
                ~expect:(Or_error.return (f xs))
                (splice xs ~span:{pos= 0; len= List.length xs} ~replace_f:f)
              )
      end )
  end )

let%test_module "try_splice" =
  ( module struct
    let test_fib ~(pos : int) ~(len : int)
        ~(replace_f : int list -> int list Or_error.t) : unit =
      test_splice_like (try_splice fibs ~span:{pos; len} ~replace_f)

    let%test_module "in-bounds" =
      ( module struct
        let test_in_bounds :
            replace_f:(int list -> int list Or_error.t) -> unit =
          test_fib ~pos:2 ~len:3

        let%expect_test "in-bounds, do nothing" =
          test_in_bounds ~replace_f:Or_error.return ;
          [%expect {| 1, 1, 2, 3, 5, 8, 13, 21 |}]

        let%expect_test "in-bounds, fail" =
          test_in_bounds ~replace_f:(Fn.const (Or_error.error_string "oops")) ;
          [%expect {| oops |}]
      end )
  end )

let%test_module "map_sub" =
  ( module struct
    let test_fib ~(pos : int) ~(len : int) ~(f : int -> int) : unit =
      test_splice_like (map_sub fibs ~span:{pos; len} ~f)

    let%test_module "in-bounds" =
      ( module struct
        let test_in_bounds : f:(int -> int) -> unit = test_fib ~pos:2 ~len:3

        let%expect_test "in-bounds, do nothing" =
          test_in_bounds ~f:Fn.id ; [%expect {| 1, 1, 2, 3, 5, 8, 13, 21 |}]

        let%expect_test "in-bounds, double items" =
          test_in_bounds ~f:(Int.( * ) 2) ;
          [%expect {| 1, 1, 4, 6, 10, 8, 13, 21 |}]
      end )

    let%test_unit "map_sub on a full list behaves as map" =
      Test.run_exn
        ( module struct
          type t = int list * (int -> int) [@@deriving sexp, quickcheck]
        end )
        ~f:(fun (xs, f) ->
          [%test_result: int list Or_error.t]
            ~here:[[%here]]
            ~equal:[%compare.equal: int list Or_error.t]
            ~expect:(Or_error.return (List.map ~f xs))
            (map_sub xs ~span:{pos= 0; len= List.length xs} ~f) )
  end )

let%test_module "try_map_sub" =
  ( module struct
    let test_fib ~(pos : int) ~(len : int) ~(f : int -> int Or_error.t) :
        unit =
      test_splice_like (try_map_sub fibs ~span:{pos; len} ~f)

    let%test_module "in-bounds" =
      ( module struct
        let test_in_bounds : f:(int -> int Or_error.t) -> unit =
          test_fib ~pos:2 ~len:3

        let%expect_test "in-bounds, do nothing" =
          test_in_bounds ~f:Or_error.return ;
          [%expect {| 1, 1, 2, 3, 5, 8, 13, 21 |}]

        let%expect_test "in-bounds, partial" =
          test_in_bounds ~f:(fun x ->
              if x % 2 = 0 then Ok x else Or_error.errorf "%d" x ) ;
          [%expect {| (3 5) |}]

        let%expect_test "in-bounds, fail" =
          test_in_bounds ~f:(Or_error.errorf "%d") ;
          [%expect {| (2 3 5) |}]
      end )
  end )

let%test_module "merge_preserving_order" =
  ( module struct
    let test (l : string) (r : string) : unit =
      let lcs = String.to_list l in
      let rcs = String.to_list r in
      let mcs = merge_preserving_order Char.equal lcs rcs in
      Stdio.print_endline (String.of_char_list mcs)

    let%expect_test "stack overflow example 1" =
      test "whijk" "awtin" ; [%expect {| awhtijkn |}]

    let%expect_test "stack overflow example 2" =
      test "whijk" "jwmna" ; [%expect {| jwhikmna |}]

    let%expect_test "stack overflow example 3" =
      test "jwmna" "whijk" ; [%expect {| whijmnak |}]

    let%expect_test "stack overflow example 4" =
      test "abcd" "efgh" ; [%expect {| abcdefgh |}]
  end )
