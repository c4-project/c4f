(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let print_flags : Set.M(Src.Path_meta.Flag).t -> unit =
  Fmt.(
    pr "@[%a@]@."
      (using (fun flags -> {Src.Path_meta.flags}) Src.Path_meta.pp))

let%test_module "check_contradiction_free" =
  ( module struct
    let test (m : Src.Path_meta.Flag.t list) : unit =
      let flags = Set.of_list (module Src.Path_meta.Flag) m in
      let meta = {Src.Path_meta.flags} in
      let result = Src.Path_meta.check_contradiction_free meta in
      Fmt.(pr "@[%a@]@." (result ~ok:nop ~error:Error.pp)) result

    let%expect_test "empty set" = test [] ; [%expect {||}]

    let%expect_test "in loop, execute-multi unsafe" =
      test [In_loop; Execute_multi_unsafe] ;
      [%expect {||}]

    let%expect_test "in execute-multi loop, execute-multi unsafe" =
      test [In_loop; In_execute_multi; Execute_multi_unsafe] ;
      [%expect
        {|
          ("Contradiction detected in path flags - possible action generator error"
           (flags (execute-multi-unsafe in-execute-multi in-loop))
           (contradictions ((execute-multi-unsafe in-execute-multi)))) |}]
  end )

let%test_module "flags_of_metadata" =
  ( module struct
    let test (m : Src.Metadata.t) : unit =
      print_flags (Src.Path_meta.flags_of_metadata m)

    let%expect_test "existing" =
      test Src.Metadata.Existing ;
      [%expect {| {execute-multi-unsafe} |}]

    let%expect_test "dead-code" =
      test Src.Metadata.gen_dead ;
      [%expect {| {in-dead-code} |}]

    let%expect_test "normal generation" =
      test Src.Metadata.gen_normal ;
      [%expect {| {} |}]

    let%expect_test "normal-with-restrictions generation" =
      test
        Src.Metadata.(
          Generated
            (Gen.make
               ~restrictions:(Set.singleton (module Restriction) Once_only)
               ())) ;
      [%expect {| {execute-multi-unsafe} |}]

    let%expect_test "once generation" =
      test Src.Metadata.gen_once ;
      [%expect {| {} |}]
  end )

let%test_module "flags_of_flow" =
  ( module struct
    let test (f : Src.Subject.Statement.Flow.t) : unit =
      print_flags (Src.Path_meta.flags_of_flow f)

    let%expect_test "generated loop" =
      test
        (Fir.Flow_block.while_loop
           ~cond:(Fir.Expression.of_variable_str_exn "foo")
           ~kind:While
           ~body:(Src.Subject.Block.make_generated ())) ;
      [%expect {| {in-execute-multi, in-loop} |}]

    let%expect_test "existing loop" =
      test
        (Fir.Flow_block.while_loop
           ~cond:(Fir.Expression.of_variable_str_exn "foo")
           ~kind:While
           ~body:(Src.Subject.Block.make_existing ())) ;
      [%expect {| {in-execute-multi, in-loop} |}]

    let%expect_test "dead loop" =
      test
        (Fir.Flow_block.while_loop
           ~cond:(Fir.Expression.of_variable_str_exn "foo")
           ~kind:While
           ~body:(Src.Subject.Block.make_dead_code ())) ;
      [%expect {| {in-loop} |}]

    let%expect_test "dead implicit" =
      test (Fir.Flow_block.implicit (Src.Subject.Block.make_dead_code ())) ;
      [%expect {| {} |}]
  end )
