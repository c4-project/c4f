(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
open Act_config
module Ac = Act_common
module Am = Act_machine

let%test_module "accessors" =
  ( module struct
    let cpp : Cpp.t = Cpp.default ()

    let fuzz : Fuzz.t = Fuzz.make ()

    let defaults : Ast.Default.t list =
      Ast.Default.
        [ Try (Compiler, Ac.Id.of_string "localhost.gcc.x86.normal")
        ; Try (Arch, Ac.Id.of_string "x86.att") ]

    let localhost_herd_spec : Act_sim.Spec.t =
      Act_sim.Spec.make ~cmd:"herd7" ~style:(Ac.Id.of_string "herd") ()

    let localhost_sims : Act_sim.Spec.Set.t =
      Or_error.ok_exn
        (Act_sim.Spec.Set.of_list
           [ Act_sim.Spec.With_id.make ~id:(Ac.Id.of_string "herd")
               ~spec:localhost_herd_spec ])

    let localhost_spec : Am.Spec.t =
      Am.Spec.make ~enabled:true ~via:Am.Via.local
        ~compilers:
          (Lazy.force Act_compiler_test.Data.Spec_sets.single_gcc_compiler)
        ~sims:localhost_sims ()

    let machines : Am.Spec.Set.t =
      Or_error.ok_exn
        (Am.Spec.Set.of_list
           [ Am.Spec.With_id.make
               ~id:(Ac.Id.of_string "localhost")
               ~spec:localhost_spec ])

    let global : Global.t = Global.make ~cpp ~fuzz ~defaults ~machines ()

    let%expect_test "cpp" =
      print_s [%sexp (Global.cpp global : Cpp.t option)] ;
      [%expect {| (((enabled true) (cmd ()) (argv ()))) |}]

    let%expect_test "defaults" =
      print_s [%sexp (Global.defaults global : Ast.Default.t list)] ;
      [%expect
        {| ((Try Compiler (localhost gcc x86 normal)) (Try Arch (x86 att))) |}]

    let%expect_test "fuzz" =
      print_s [%sexp (Global.fuzz global : Fuzz.t option)] ;
      [%expect {| (((weights ()))) |}]

    let%expect_test "machines" =
      Fmt.pr "@[%a@]@." Am.Spec.Set.pp
        (Global.machines global) ;
      [%expect {| localhost: local |}]
  end )
