(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck
open Stdio
module Src = Act_fuzz

module Dummy_payload = struct
  type t = {foo: int; bar: bool; baz: string} [@@deriving sexp, quickcheck]
end

module Dummy_action :
  Src.Action_types.S with type Payload.t = Dummy_payload.t = struct
  let name = Act_common.Id.of_string "dummy.action"

  let available = Src.Availability.always

  let readme () =
    {| This is a module that is almost, but not quite, entirely unlike a fuzzer action. |}

  module Payload = Src.Payload.Pure (Dummy_payload)

  let run subject ~(payload : Dummy_payload.t) =
    ignore payload ;
    Src.State.Monad.return subject
end

module Another_dummy_action : Src.Action_types.S with type Payload.t = unit =
struct
  let name = Act_common.Id.of_string "another.dummy.action"

  let available = Src.Availability.always

  let readme () =
    {| This is also a module that is almost, but not quite, entirely unlike a fuzzer action. |}

  module Payload = Src.Payload.None

  let run subject ~(payload : unit) =
    ignore payload ;
    Src.State.Monad.return subject
end

let example_trace : Src.Trace.t Lazy.t =
  lazy
    Src.Trace.(
      empty
      |> add
           ~action:(module Dummy_action)
           ~payload:{foo= 27; bar= true; baz= "hello"}
      |> add ~action:(module Another_dummy_action) ~payload:()
      |> add
           ~action:(module Dummy_action)
           ~payload:{foo= 53; bar= false; baz= "world"})

let%test_module "S-expression representation" =
  ( module struct
    let%expect_test "empty trace" =
      print_s [%sexp (Src.Trace.empty : Src.Trace.t)] ;
      [%expect {| () |}]

    let%expect_test "example trace" =
      print_s [%sexp (Lazy.force example_trace : Src.Trace.t)] ;
      [%expect
        {|
      (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))
       ((name (another dummy action)) (payload ()))
       ((name (dummy action)) (payload ((foo 53) (bar false) (baz world))))) |}]
  end )

let%test_module "trace playback" =
  ( module struct
    let%expect_test "empty trace does nothing" =
      let test = Lazy.force Subject.Test_data.test in
      let initial_state = Lazy.force Subject.Test_data.state in
      let computation =
        Src.Trace.(
          run empty test ~resolve:(fun _ ->
              Or_error.unimplemented "shouldn't run this"))
      in
      Action.Test_utils.run_and_dump_test computation ~initial_state ;
      [%expect
        {|
      void
      P0(atomic_int *x, atomic_int *y)
      {
          atomic_int r0 = 4004;
          atomic_store_explicit(x, 42, memory_order_seq_cst);
          ;
          atomic_store_explicit(y, foo, memory_order_relaxed);
          if (foo == y)
          { atomic_store_explicit(x, 56, memory_order_seq_cst); kappa_kappa: ; }
          if (false)
          {
              atomic_store_explicit(y,
                                    atomic_load_explicit(x, memory_order_seq_cst),
                                    memory_order_seq_cst);
          }
          do { atomic_store_explicit(x, 44, memory_order_seq_cst); } while (4 ==
          5);
      }

      void
      P1(atomic_int *x, atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } } |}]
  end )

let%test_module "trace bisection" =
  ( module struct
    let test ~(want : [`Last_on_left | `First_on_right])
        ~(f : Src.Trace.t -> [`Left | `Right]) : unit =
      print_s
        [%sexp
          (Src.Trace.bisect ~want ~f (Lazy.force example_trace) : Src.Trace.t)]

    let%expect_test "left bisection always returning left returns full trace"
        =
      test ~want:`Last_on_left ~f:(Fn.const `Left) ;
      [%expect
        {|
        (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))
         ((name (another dummy action)) (payload ()))
         ((name (dummy action)) (payload ((foo 53) (bar false) (baz world))))) |}]

    let%expect_test "right bisection always returning left returns full \
                     trace" =
      test ~want:`First_on_right ~f:(Fn.const `Left) ;
      [%expect
        {|
        (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))
         ((name (another dummy action)) (payload ()))
         ((name (dummy action)) (payload ((foo 53) (bar false) (baz world))))) |}]

    let%expect_test "left bisection always returning right returns empty \
                     trace" =
      test ~want:`Last_on_left ~f:(Fn.const `Right) ;
      [%expect {|
        () |}]

    let%expect_test "right bisection always returning right returns trace \
                     of length 1" =
      test ~want:`First_on_right ~f:(Fn.const `Right) ;
      [%expect
        {|
        (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))) |}]

    let towards_length (n : int) (t : Src.Trace.t) : [`Left | `Right] =
      if n < Src.Trace.length t then `Right else `Left

    let%expect_test "left bisection towards length 0 returns trace of \
                     length 0" =
      test ~want:`Last_on_left ~f:(towards_length 0) ;
      [%expect {| () |}]

    let%expect_test "right bisection towards length 0 returns trace of \
                     length 1" =
      test ~want:`First_on_right ~f:(towards_length 0) ;
      [%expect
        {| (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))) |}]

    let%expect_test "left bisection towards length 1 returns trace of \
                     length 1" =
      test ~want:`Last_on_left ~f:(towards_length 1) ;
      [%expect
        {| (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))) |}]

    let%expect_test "right bisection towards length 1 returns trace of \
                     length 2" =
      test ~want:`First_on_right ~f:(towards_length 1) ;
      [%expect
        {|
        (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))
         ((name (another dummy action)) (payload ()))) |}]

    let%expect_test "left bisection towards length 2 returns trace of \
                     length 2" =
      test ~want:`Last_on_left ~f:(towards_length 2) ;
      [%expect
        {|
        (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))
         ((name (another dummy action)) (payload ()))) |}]

    let%expect_test "right bisection towards length 2 returns trace of \
                     length 3" =
      test ~want:`First_on_right ~f:(towards_length 2) ;
      [%expect
        {|
        (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))
         ((name (another dummy action)) (payload ()))
         ((name (dummy action)) (payload ((foo 53) (bar false) (baz world))))) |}]

    let%expect_test "left bisection towards length 3 returns trace of \
                     length 3" =
      test ~want:`Last_on_left ~f:(towards_length 3) ;
      [%expect
        {|
        (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))
         ((name (another dummy action)) (payload ()))
         ((name (dummy action)) (payload ((foo 53) (bar false) (baz world))))) |}]

    let%expect_test "right bisection towards length 3 returns full trace" =
      test ~want:`First_on_right ~f:(towards_length 3) ;
      [%expect
        {|
        (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))
         ((name (another dummy action)) (payload ()))
         ((name (dummy action)) (payload ((foo 53) (bar false) (baz world))))) |}]

    let%expect_test "left bisection towards length 4 returns full trace" =
      test ~want:`Last_on_left ~f:(towards_length 4) ;
      [%expect
        {|
        (((name (dummy action)) (payload ((foo 27) (bar true) (baz hello))))
         ((name (another dummy action)) (payload ()))
         ((name (dummy action)) (payload ((foo 53) (bar false) (baz world))))) |}]
  end )
