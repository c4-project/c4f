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

open Core
open Act_common

(* Module shorthand *)
module C_spec = Act_compiler.Spec
module Cq_spec = Act_machine.Qualified.Compiler

let litmusify o (passes : Set.M(Act_sanitiser.Pass_group).t) spec c_file =
  let target = `Spec spec in
  let config = Act_asm.Litmusifier.Config.make ~format:Programs_only () in
  let litmus_job = Act_asm.Job.make ~config ~passes () in
  let job_input =
    Fn.const
      (Act_compiler.Filter.Chain_input.make ~mode:Assembly ~file_type:C
         ~next:(Fn.const litmus_job))
  in
  let input = Act_asm.Pipeline.Input.make ~file_type:C ~job_input in
  Or_error.Let_syntax.(
    let%bind (module Comp_lit) = Common.litmusify_pipeline target in
    let%map out =
      Comp_lit.run input (Plumbing.Input.file c_file) Plumbing.Output.stdout
    in
    Output.pw o "@[%a@]@." Act_asm.Job.Output.warn
      (Act_asm.Pipeline.Output.job_output out))

let run_spec_on_file o passes (spec : Cq_spec.t) ~c_file =
  Format.printf "@[<v>@,@[<h>##@ %a@]@,@,```@]@." Id.pp
    (C_spec.With_id.id (Cq_spec.c_spec spec)) ;
  let open Or_error.Let_syntax in
  let%map _ = litmusify o passes spec c_file in
  Format.printf "@[<h>```@]@."

let run o cfg ~(c_file_raw : string) =
  let open Or_error.Let_syntax in
  let%bind c_file = Plumbing.Fpath_helpers.of_string c_file_raw in
  let specs = Act_config.Act.all_compilers cfg in
  let passes =
    Act_config.Act.sanitiser_passes cfg
      ~default:Act_sanitiser.Pass_group.standard
  in
  Fmt.pr "@[<h>#@ %a@]@." Fpath.pp c_file ;
  Or_error.combine_errors_unit
    (List.map specs ~f:(run_spec_on_file o passes ~c_file))

let command : Command.t =
  Command.basic
    ~summary:"displays the litmus output for each compiler over a C file"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard.get
      and sanitiser_passes = Args.sanitiser_passes
      and compiler_predicate = Args.compiler_predicate
      and machine_predicate = Args.machine_predicate
      and c_file_raw = anon ("FILE" %: Filename.arg_type) in
      fun () ->
        Common.lift_command standard_args ?compiler_predicate
          ?machine_predicate ?sanitiser_passes ~with_compiler_tests:true
          ~f:(fun _args -> run ~c_file_raw))
