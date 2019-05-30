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
    ; user_cvars: Ac.C_variables.Map.t option
    ; target: Act_compiler.Instance.Target.t
    ; pb_input: Plumbing.Input.t
    ; pb_output: Plumbing.Output.t
    ; output: Act_common.Output.t }
  [@@deriving fields]

  let file_type (i : t) : Act_common.File_type.t =
    Args.Standard_asm.file_type (args i)

  let make_compiler_input (i : t)
      (config_builder :
        c_variables:Act_common.C_variables.Map.t option -> 'cfg) :
         Act_c.Filters.Output.t Pb.Chain_context.t
      -> 'cfg Act_asm.Job.t Act_compiler.Instance.Chain_input.t =
    Common.make_compiler_input (output i) (file_type i) (user_cvars i)
      config_builder (sanitiser_passes i)
end

let resolve_target (args : Args.Standard_asm.t) (cfg : Act_config.Act.t) :
    Act_compiler.Instance.Target.t Or_error.t =
  let raw_target = Args.Standard_asm.target args in
  Asm_target.resolve ~cfg raw_target

let collect_cvars (args : Args.Standard_asm.t) :
    Ac.C_variables.Map.t option Or_error.t =
  let c_globals = Args.Standard_asm.c_globals args in
  let c_locals = Args.Standard_asm.c_locals args in
  let module V = Ac.C_variables in
  Or_error.Let_syntax.(
    let%bind globals =
      Tx.Option.With_errors.map_m
        ~f:(V.String_lang.parse_list ~scope:V.Scope.Global)
        c_globals
    in
    let%map locals =
      Tx.Option.With_errors.map_m
        ~f:(V.String_lang.parse_list ~scope:V.Scope.Local)
        c_locals
    in
    Option.merge globals locals ~f:(fun x y -> V.Map.merge_list [x; y]))

let with_input (args : Args.Standard_asm.t) (output : Ac.Output.t)
    (act_config : Act_config.Act.t) ~(f : Input.t -> unit Or_error.t)
    ~(default_passes : Set.M(Act_sanitiser.Pass_group).t) : unit Or_error.t
    =
  let sanitiser_passes =
    Act_config.Act.sanitiser_passes act_config ~default:default_passes
  in
  Or_error.Let_syntax.(
    let%bind user_cvars = collect_cvars args in
    let%bind target = resolve_target args act_config in
    let%bind pb_input = Args.Standard_asm.infile_source args in
    let%bind pb_output = Args.Standard_asm.outfile_sink args in
    let input =
      Input.Fields.create ~act_config ~output ~args ~sanitiser_passes
        ~user_cvars ~target ~pb_input ~pb_output
    in
    f input)

let lift_command (args : Args.Standard_asm.t)
    ~(f : Input.t -> unit Or_error.t)
    ~(default_passes : Set.M(Act_sanitiser.Pass_group).t) : unit =
  Common.lift_asm_command_basic args ~f:(with_input ~f ~default_passes)
