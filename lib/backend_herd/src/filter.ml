(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Stdio
open Act_common
module Pb = Plumbing

let model_for_arch (config : Act_backend.Spec.t) :
    Act_backend.Arch.t -> string option = function
  | C ->
      Act_backend.Spec.c_model config
  | Assembly emits_spec ->
      List.Assoc.find
        (Act_backend.Spec.asm_models config)
        emits_spec ~equal:[%equal: Id.t]

let make_argv ?(model : string option) (rest : string list) =
  Option.value_map model ~f:(fun m -> ["-model"; m]) ~default:[] @ rest

let%expect_test "make_argv: no model" =
  let argv = make_argv ["herd7"] in
  print_s [%sexp (argv : string list)] ;
  [%expect {| (herd7) |}]

let%expect_test "make_argv: override model" =
  let argv = make_argv ~model:"c11_lahav.cat" ["herd7"] in
  print_s [%sexp (argv : string list)] ;
  [%expect {| (-model c11_lahav.cat herd7) |}]

let make_argv_from_config (config : Act_backend.Spec.t)
    (arch : Act_backend.Arch.t option) (rest : string list) =
  let model = Option.bind ~f:(model_for_arch config) arch in
  make_argv ?model rest

let run_direct ?(arch : Act_backend.Arch.t option)
    ?(oc : Out_channel.t = Out_channel.stdout) (config : Act_backend.Spec.t)
    (argv : string list) : unit Or_error.t =
  let prog = Act_backend.Spec.cmd config in
  let argv' = make_argv_from_config config arch argv in
  Or_error.tag ~tag:"While running herd"
    (Plumbing.Runner.Local.run ~oc ~prog argv')

module Make (B : Act_backend.Runner_types.Basic) : Act_backend.Filter.S =
Plumbing.Filter.Make_on_runner (struct
  (* TODO(@MattWindsor91): this invariably can now be rolled into Sim_litmus *)
  module Runner = B.Runner

  type aux_i = Act_backend.Arch.t

  let name = "Herd tool"

  let prog _t = Act_backend.Spec.cmd B.spec

  let argv t path = make_argv_from_config B.spec (Some t) [path]
end)
