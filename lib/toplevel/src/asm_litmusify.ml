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

open Core_kernel
module A = Act_common
module Tx = Travesty_base_exts

let litmus_config_fn (aux : Act_delitmus.Aux.t) :
    Act_asm.Litmusifier.Config.t =
  Act_asm.Litmusifier.Config.make ~aux ()

module In = Asm_common.Input

let run (input : In.t) : unit Or_error.t =
  let cfg = In.act_config input in
  let infile = In.pb_input input in
  let outfile = In.pb_output input in
  let target = In.target input in
  let module R = Sim_support.Make_resolver (struct
    let cfg = cfg
  end) in
  Or_error.Let_syntax.(
    let%bind (module Runner) = Common.asm_runner_of_target target in
    let module Lit = Act_asm.Litmusifier.Make (Runner) in
    let job_input = In.make_job_input input litmus_config_fn in
    Or_error.ignore_m (Lit.Filter.run job_input infile outfile))

let command =
  Command.basic ~summary:"converts an assembly file to a litmus test"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard_asm.get in
      fun () ->
        Asm_common.lift_command standard_args ~f:run
          ~default_passes:Act_sanitiser.Pass_group.standard)
