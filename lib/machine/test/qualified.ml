(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

let print_result (type a) (pp_inner : a Fmt.t) : a Or_error.t -> unit =
  Fmt.(pr "@[%a@]@." (result ~ok:pp_inner ~error:Error.pp))

let%test_module "compiler lookup" =
  ( module struct
    let machines = Lazy.force Data.Spec_sets.single_local_machine

    let test ?(defaults : Act_common.Id.t list option)
        (fqid : Act_common.Id.t) : unit =
      let result =
        Act_machine.Qualified.Lookup_compilers.lookup machines ~fqid
          ?defaults
      in
      print_result
        (Fmt.using Act_machine.Qualified.Compiler.c_spec
           Act_compiler.Spec.With_id.pp)
        result

    let%expect_test "positive example" =
      test (Act_common.Id.of_string "localhost.gcc.x86.normal") ;
      [%expect
        {|
        Style: gcc
        Emits: x86.att
        Command: gcc |}]

    let%expect_test "positive example: defaults resolution" =
      test
        (Act_common.Id.of_string "gcc.x86.normal")
        ~defaults:
          Act_common.Id.
            [of_string "foo"; of_string "bar"; of_string "localhost"] ;
      [%expect
        {|
        Style: gcc
        Emits: x86.att
        Command: gcc |}]

    let%expect_test "negative example: wrong compiler" =
      test (Act_common.Id.of_string "localhost.clang.x86.O3") ;
      [%expect
        {|
        ("unknown ID" (of_type compiler) (id (clang x86 O3))
         (suggestions ((gcc x86 normal)))) |}]

    let%expect_test "negative example: wrong machine" =
      test (Act_common.Id.of_string "kappa.gcc.x86.normal") ;
      [%expect
        {|
        ("unknown ID" (of_type machine) (id (kappa gcc x86 normal))
         (suggestions ((localhost)))) |}]

    let%expect_test "negative example: missing machine" =
      test (Act_common.Id.of_string "gcc.x86.normal") ;
      [%expect
        {|
        ("unknown ID" (of_type machine) (id (gcc x86 normal))
         (suggestions ((localhost)))) |}]

    let%expect_test "negative example: failed defaults resolution" =
      test
        (Act_common.Id.of_string "gcc.x86.normal")
        ~defaults:
          Act_common.Id.
            [of_string "foo"; of_string "bar"; of_string "kappa"] ;
      [%expect
        {|
        ("unknown ID" (of_type machine) (id (gcc x86 normal))
         (suggestions ((localhost)))) |}]
  end )

let%test_module "sim lookup" =
  ( module struct
    let machines = Lazy.force Data.Spec_sets.single_local_machine

    let test (fqid : Act_common.Id.t) : unit =
      let result =
        Act_machine.Qualified.Lookup_sims.lookup machines ~fqid
      in
      print_result
        (Fmt.using Act_machine.Qualified.Sim.s_spec Act_backend.Spec.With_id.pp)
        result

    let%expect_test "positive example" =
      test (Act_common.Id.of_string "localhost.herd") ;
      [%expect {| TODO |}]

    let%expect_test "negative example" =
      test (Act_common.Id.of_string "localhost.litmus") ;
      [%expect
        {| ("unknown ID" (of_type sim) (id (litmus)) (suggestions ((herd)))) |}]

    let%expect_test "negative example: wrong machine" =
      test (Act_common.Id.of_string "kappa.herd") ;
      [%expect
        {|
        ("unknown ID" (of_type machine) (id (kappa herd))
         (suggestions ((localhost)))) |}]
  end )
