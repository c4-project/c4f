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
module Ac = Act_common

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
The `asm gen-stubs` command generates GCC assembly directives that can
be slotted into a simulation harness.
|}

module In = Asm_common.Input

let make_config ~(c_variables : Ac.C_variables.Map.t option) :
    Act_asm.Stub_gen.Config.t =
  ignore (c_variables : Ac.C_variables.Map.t option) ;
  Act_asm.Stub_gen.Config.make ~separator:"// NEXT" ()

let stub_gen_runner (module B : Act_asm.Runner_intf.Basic) :
    (module Act_asm.Stub_gen.S_filter) =
  let module Sg = Act_asm.Stub_gen.Make (B) in
  (module Sg.Filter)

let stub_gen_filter (target : Act_config.Compiler.Target.t) :
    (module Act_utils.Filter.S
       with type aux_i = Act_common.File_type.t
                         * (   Act_c.Filters.Output.t
                               Act_utils.Filter.chain_output
                            -> Act_asm.Stub_gen.Config.t Act_asm.Job.t
                               Act_config.Compiler.Chain_input.t)
        and type aux_o = Act_c.Filters.Output.t option
                         * (unit option * Act_asm.Job.Output.t))
    Or_error.t =
  Or_error.tag ~tag:"while getting a stub-gen filter for this target"
    (Common.delitmus_compile_asm_pipeline target stub_gen_runner)

let run (input : In.t) : unit Or_error.t =
  let file_type = In.file_type input in
  let infile = In.infile_raw input in
  let outfile = In.outfile_raw input in
  let compiler_input_fn = In.make_compiler_input input make_config in
  let open Or_error.Let_syntax in
  let%bind (module Sg) = stub_gen_filter (In.target input) in
  Or_error.ignore_m
    (Sg.run_from_string_paths
       (file_type, compiler_input_fn)
       ~infile ~outfile)

let command : Command.t =
  Command.basic ~summary:"generates GCC asm stubs from an assembly file"
    ~readme
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard_asm.get in
      fun () ->
        Asm_common.lift_command standard_args ~f:run
          ~default_passes:Act_sanitiser.Pass_group.light)
