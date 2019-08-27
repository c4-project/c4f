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

module Data = struct
  let act : Act_config.Act.t Lazy.t =
    Lazy.Let_syntax.(
      let%map global = Global.Data.global in
      let machines = Act_config.Global.machines global in
      Act_config.Act.make ~global ~machines ())
end

let%test_module "compiler lookup" =
  ( module struct
    let act = Lazy.force Data.act

    (* This is similar to the tests in Act.Machine.Qualified, and will
       likely need changing if that module changes. *)

    let test (fqid : Act_common.Id.t) : unit =
      let result = Act_config.Act.compiler act ~fqid in
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

    let%expect_test "positive example: expected defaults resolution" =
      test (Act_common.Id.of_string "gcc.x86.normal") ;
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
  end )
