(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Base
open Stdio
open Act_common
open Filter_intf
module Pb = Plumbing

let model_for_arch (config : Act_config.Herd.t) :
    Act_sim.Arch.t -> string option = function
  | C ->
      Act_config.Herd.c_model config
  | Assembly emits_spec ->
      List.Assoc.find
        (Act_config.Herd.asm_models config)
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

let make_argv_from_config (config : Act_config.Herd.t)
    (arch : Act_sim.Arch.t option) (rest : string list) =
  let model = Option.bind ~f:(model_for_arch config) arch in
  make_argv ?model rest

let run_direct ?(arch : Act_sim.Arch.t option)
    ?(oc : Out_channel.t = Out_channel.stdout) (config : Act_config.Herd.t)
    (argv : string list) : unit Or_error.t =
  let prog = Act_config.Herd.cmd config in
  let argv' = make_argv_from_config config arch argv in
  Or_error.tag ~tag:"While running herd"
    (Plumbing.Runner.Local.run ~oc ~prog argv')

module Make (B : Basic) :
  Pb.Filter.S with type aux_i = Act_sim.Arch.t and type aux_o = unit =
Plumbing.Filter.Make_on_runner (struct
  module Runner = Plumbing.Runner.Local

  type aux_i = Act_sim.Arch.t

  let name = "Herd tool"

  let tmp_file_ext = Fn.const "txt"

  let prog _t = Act_config.Herd.cmd B.config

  let argv t path = make_argv_from_config B.config (Some t) [path]
end)
