(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Src = Act_litmus
module Test = Src.Test

let dump_test (v : (int, string) Test.Raw.t Or_error.t) : unit =
  Stdio.print_s [%sexp (v : (int, string) Test.Raw.t Or_error.t)]

let%test_module "SBSC example" =
  ( module struct
    let test : (int, string) Test.Raw.t = Lazy.force Examples.Sbsc.test

    let%expect_test "number of threads" =
      Stdio.printf "%d" (Test.Raw.num_threads test) ;
      [%expect {| 2 |}]

    let%expect_test "has threads" =
      Stdio.printf "%b" (Test.Raw.has_threads test) ;
      [%expect {| true |}]

    let%expect_test "witness example before modifications" =
      dump_test (Or_error.return test) ;
      [%expect
        {|
      (Ok
       ((header
         ((locations ((x y))) (init ((x 0) (y 0)))
          (postcondition
           (((quantifier exists)
             (predicate (Bop And (Elt (Eq 0:a 0)) (Elt (Eq 1:a 1)))))))
          (name SBSC)))
        (threads ("(this was thread 0)" "(this was thread 1)")))) |}]

    let%expect_test "add_thread_at_end" =
      dump_test
        (Or_error.return
           (Test.Raw.add_thread_at_end test ~thread:"<NEW THREAD>")) ;
      [%expect
        {|
      (Ok
       ((header
         ((locations ((x y))) (init ((x 0) (y 0)))
          (postcondition
           (((quantifier exists)
             (predicate (Bop And (Elt (Eq 0:a 0)) (Elt (Eq 1:a 1)))))))
          (name SBSC)))
        (threads ("(this was thread 0)" "(this was thread 1)" "<NEW THREAD>")))) |}]

    let%expect_test "add_thread before existing threads" =
      dump_test (Test.Raw.add_thread test ~index:0 ~thread:"<NEW THREAD>") ;
      [%expect
        {|
      (Ok
       ((header
         ((locations ((x y))) (init ((x 0) (y 0)))
          (postcondition
           (((quantifier exists)
             (predicate (Bop And (Elt (Eq 1:a 0)) (Elt (Eq 2:a 1)))))))
          (name SBSC)))
        (threads ("<NEW THREAD>" "(this was thread 0)" "(this was thread 1)")))) |}]

    let%expect_test "add_thread in between existing threads" =
      dump_test (Test.Raw.add_thread test ~index:1 ~thread:"<NEW THREAD>") ;
      [%expect
        {|
      (Ok
       ((header
         ((locations ((x y))) (init ((x 0) (y 0)))
          (postcondition
           (((quantifier exists)
             (predicate (Bop And (Elt (Eq 0:a 0)) (Elt (Eq 2:a 1)))))))
          (name SBSC)))
        (threads ("(this was thread 0)" "<NEW THREAD>" "(this was thread 1)")))) |}]

    let%expect_test "add_thread after existing threads" =
      dump_test (Test.Raw.add_thread test ~index:2 ~thread:"<NEW THREAD>") ;
      [%expect
        {|
      (Ok
       ((header
         ((locations ((x y))) (init ((x 0) (y 0)))
          (postcondition
           (((quantifier exists)
             (predicate (Bop And (Elt (Eq 0:a 0)) (Elt (Eq 1:a 1)))))))
          (name SBSC)))
        (threads ("(this was thread 0)" "(this was thread 1)" "<NEW THREAD>")))) |}]

    let%expect_test "add_thread: out of bounds" =
      dump_test (Test.Raw.add_thread test ~index:3 ~thread:"<NEW THREAD>") ;
      [%expect
        {|
      (Error
       ("Insert failed: index out of range" (here base_exts/src/list.ml:90:54)
        (insert_at 3) (list_length 2))) |}]

    let%expect_test "add_thread, then remove it" =
      dump_test
        Or_error.Let_syntax.(
          test
          |> Test.Raw.add_thread ~index:0 ~thread:"<NEW THREAD>"
          >>= Test.Raw.remove_thread ~index:0) ;
      [%expect
        {|
      (Ok
       ((header
         ((locations ((x y))) (init ((x 0) (y 0)))
          (postcondition
           (((quantifier exists)
             (predicate (Bop And (Elt (Eq 0:a 0)) (Elt (Eq 1:a 1)))))))
          (name SBSC)))
        (threads ("(this was thread 0)" "(this was thread 1)")))) |}]
  end )
