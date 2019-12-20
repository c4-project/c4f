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

module Data = struct
  let fuzz : Act_fuzz.Config.t Lazy.t = Lazy.from_fun Act_fuzz.Config.make

  (* These defaults should line up with the ones in Machine.Test.Qualified.

     TODO(@MattWindsor91): unify them. *)

  let defaults : Default.t Lazy.t =
    Lazy.from_fun
      Ac.Id.(
        Default.make
          ~machines:[of_string "foo"; of_string "bar"; of_string "localhost"]
          ~compilers:[of_string "localhost.gcc.x86.normal"]
          ~arches:[of_string "x86.att"])

  let machines : Am.Spec.Set.t Lazy.t =
    Act_machine_test.Data.Spec_sets.single_local_machine

  let global : Global.t Lazy.t =
    Lazy.Let_syntax.(
      let%map fuzz = fuzz and defaults = defaults and machines = machines in
      Global.make ~fuzz ~defaults ~machines ())
end

let%test_module "accessors" =
  ( module struct
    let global = Lazy.force Data.global

    let%expect_test "defaults" =
      print_s [%sexp (Global.defaults global : Default.t)] ;
      [%expect
        {|
          ((arches ((x86 att))) (compilers ((localhost gcc x86 normal)))
           (machines ((foo) (bar) (localhost))) (backends ())) |}]

    let%expect_test "fuzz" =
      print_s [%sexp (Global.fuzz global : Act_fuzz.Config.t)] ;
      [%expect {| ((weights ()) (max_passes 25)) |}]

    let%expect_test "machines" =
      let machines =
        global |> Global.machines |> Act_common.Spec.Set.to_list
      in
      Fmt.(
        pr "@[%a@]@."
          (list
             (concat ~sep:(any ":@ ")
                [ using Act_common.Spec.With_id.id Act_common.Id.pp
                ; using Act_common.Spec.With_id.spec Act_machine.Spec.pp ]))
          machines) ;
      [%expect {| localhost: local |}]
  end )
