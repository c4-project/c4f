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

include Base
module Tx = Travesty_base_exts
module Ac = Act_common
module Pb = Plumbing

module Input = struct
  type t =
    { act_config: Act_config.Act.t
    ; args: Args.Standard_asm.t
    ; sanitiser_passes: Set.M(Act_sanitiser.Pass_group).t
    ; c_litmus_aux: Act_delitmus.Output.Aux.t
    ; target: Act_machine.Target.t
    ; pb_input: Plumbing.Input.t
    ; pb_output: Plumbing.Output.t
    ; output: Act_common.Output.t }
  [@@deriving fields]

  let make_job_input (i : t)
      (config_fn : Act_delitmus.Output.Aux.t -> 'cfg) : 'cfg Act_asm.Job.t
      =
    let aux = c_litmus_aux i in
    let config = config_fn aux in
    Act_asm.Job.make ~config ~passes:(sanitiser_passes i) ~aux ()
end

let resolve_target (args : Args.Standard_asm.t) (cfg : Act_config.Act.t) :
    Act_machine.Target.t Or_error.t =
  let raw_target = Args.Standard_asm.target args in
  Asm_target.resolve ~cfg raw_target

let get_aux (_args : Args.Standard_asm.t) : Act_delitmus.Output.Aux.t Or_error.t =
  Stdio.eprintf "FIXME: aux input isn't supported yet.";
  Or_error.return Act_delitmus.Output.Aux.empty

let with_input (args : Args.Standard_asm.t) (output : Ac.Output.t)
    (act_config : Act_config.Act.t) ~(f : Input.t -> unit Or_error.t)
    ~(default_passes : Set.M(Act_sanitiser.Pass_group).t) : unit Or_error.t
    =
  let sanitiser_passes =
    Act_config.Act.sanitiser_passes act_config ~default:default_passes
  in
  Or_error.Let_syntax.(
    let%bind c_litmus_aux = get_aux args in
    let%bind target = resolve_target args act_config in
    let%bind pb_input = Args.Standard_asm.infile_source args in
    let%bind pb_output = Args.Standard_asm.outfile_sink args in
    let input =
      Input.Fields.create ~act_config ~output ~args ~sanitiser_passes
        ~c_litmus_aux ~target ~pb_input ~pb_output
    in
    f input)

let lift_command (args : Args.Standard_asm.t)
    ~(f : Input.t -> unit Or_error.t)
    ~(default_passes : Set.M(Act_sanitiser.Pass_group).t) : unit =
  Common.lift_asm_command_basic args ~f:(with_input ~f ~default_passes)
