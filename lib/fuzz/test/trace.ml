(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import
open Stdio

module Dummy_payload = struct
  open Base_quickcheck

  type t = {foo: int; bar: bool; baz: string} [@@deriving sexp, quickcheck]
end

module Dummy_action :
  Src.Action_types.S with type Payload.t = Dummy_payload.t = struct
  let name = C4f_common.Id.of_string "dummy.action"

  let recommendations (_ : Dummy_payload.t) = []

  let available = Src.Availability.always

  let readme =
    lazy
      {| This is a module that is almost, but not quite, entirely unlike a fuzzer action. |}

  module Payload = Src.Payload_impl.Pure (Dummy_payload)

  let run subject ~(payload : Dummy_payload.t) =
    ignore payload ;
    Src.State.Monad.return subject
end

module Another_dummy_action : Src.Action_types.S with type Payload.t = unit =
struct
  let name = C4f_common.Id.of_string "another.dummy.action"

  let recommendations () = []

  let available = Src.Availability.always

  let readme =
    lazy
      {| This is also a module that is almost, but not quite, entirely unlike a fuzzer action. |}

  module Payload = Src.Payload_impl.None

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
      |> add ~action:(module Src.Action.Nop) ~payload:()
      |> add
           ~action:(module Dummy_action)
           ~payload:{foo= 53; bar= false; baz= "world"})

let%test_module "S-expression representation" =
  ( module struct
    let%expect_test "empty trace" =
      print_s [%sexp (Src.Trace.empty : Src.Trace.t)] ;
      [%expect {| () |}]

    let%expect_test "example trace" =
      (* The nop shouldn't turn up in the trace. *)
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
      let initial_state = Lazy.force State.Test_data.state in
      let computation =
        Src.Trace.(
          run empty test ~resolve:(fun _ ->
              Or_error.unimplemented "shouldn't run this" ))
      in
      Action.Test_utils.run_and_dump_test computation ~initial_state ;
      [%expect
        {|
      void
      P0(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
         bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
         atomic_int *y)
      {
          atomic_int r0 = 4004;
          int r1 = 8008;
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
          for (r1 = 0; r1 <= 2; r1++)
          { atomic_store_explicit(x, 99, memory_order_seq_cst); }
          while (4 == 5) { atomic_store_explicit(x, 44, memory_order_seq_cst); }
      }

      void
      P1(bool a, atomic_bool b, atomic_int bar, bool barbaz, atomic_int *baz,
         bool c, int d, int e, int foo, atomic_bool foobar, atomic_int *x,
         atomic_int *y)
      { loop: ; if (true) {  } else { goto loop; } }

      Vars:
        a: bool, =false, @global, generated, []
        b: atomic_bool, =true, @global, generated, []
        bar: atomic_int, =?, @global, existing, []
        barbaz: bool, =?, @global, existing, []
        baz: atomic_int*, =?, @global, existing, []
        c: bool, =?, @global, generated, []
        d: int, =?, @global, existing, []
        e: int, =?, @global, generated, []
        foo: int, =?, @global, existing, []
        foobar: atomic_bool, =?, @global, existing, []
        x: atomic_int*, =27, @global, generated, []
        y: atomic_int*, =53, @global, generated, []
        0:r0: atomic_int, =4004, @P0, generated, []
        0:r1: int, =8008, @P0, generated, []
        1:r0: bool, =?, @P1, existing, []
        1:r1: int, =?, @P1, existing, []
        2:r0: int, =?, @P2, existing, []
        2:r1: bool, =?, @P2, existing, []
        3:r0: int*, =?, @P3, existing, [] |}]
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

    let%expect_test "right bisection always returning right returns empty \
                     trace" =
      test ~want:`First_on_right ~f:(Fn.const `Right) ;
      [%expect {|
        () |}]

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
