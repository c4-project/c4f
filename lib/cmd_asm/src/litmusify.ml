(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module A = Act_common
module Tx = Travesty_base_exts

let litmus_config_fn (aux : Act_delitmus.Aux.t) :
    Act_asm.Litmusifier.Config.t =
  Act_asm.Litmusifier.Config.make ~aux ()

module In = Common.Input

let run (input : In.t) : unit Or_error.t =
  let cfg = In.act_config input in
  let infile = In.pb_input input in
  let outfile = In.pb_output input in
  let target = In.target input in
  let module R = Toplevel.Sim_support.Make_resolver (struct
    let cfg = cfg
  end) in
  Or_error.Let_syntax.(
    let%bind (module Runner) =
      Toplevel.Common.asm_runner_of_target target
    in
    let module Lit = Act_asm.Litmusifier.Make (Runner) in
    let job_input = In.make_job_input input litmus_config_fn in
    Or_error.ignore_m (Lit.Filter.run job_input infile outfile))

let command =
  Command.basic ~summary:"converts an assembly file to a litmus test"
    Command.Let_syntax.(
      let%map standard_args = Args.Standard_asm.get in
      fun () ->
        Common.lift_command standard_args ~f:run
          ~default_passes:Act_sanitiser.Pass_group.standard)
