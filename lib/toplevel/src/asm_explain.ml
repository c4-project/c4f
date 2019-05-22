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

let print_symbol_map = function
  | [] ->
      ()
  | map ->
      Fmt.(
        pr "@[<v>@,Symbol map:@,@,%a@]@."
          (list ~sep:sp (fun f (k, v) -> pf f "@[<hv>%s@ ->@ %s@]" k v))
          map)

let explain_runner (module B : Act_asm.Runner_intf.Basic) :
    (module Act_asm.Explainer.S_filter) =
  let module Exp = Act_asm.Explainer.Make (B) in
  (module Exp.Filter)

let explain_filter (target : Act_config.Compiler.Target.t) :
    (module Act_utils.Filter.S
       with type aux_i = Act_common.File_type.t
                         * (   Act_c.Filters.Output.t
                               Act_utils.Filter.chain_output
                            -> Act_asm.Explainer.Config.t Act_asm.Job.t
                               Act_config.Compiler.Chain_input.t)
        and type aux_o = Act_c.Filters.Output.t option
                         * (unit option * Act_asm.Job.Output.t))
    Or_error.t =
  Or_error.tag ~tag:"while getting an explain filter for this target"
    (Common.delitmus_compile_asm_pipeline target explain_runner)

let run_with_input_fn (o : A.Output.t) (file_type : Act_common.File_type.t)
    target compiler_input_fn infile outfile =
  Or_error.Let_syntax.(
    let%bind (module Exp) = explain_filter target in
    A.Output.pv o "Got explain filter (name %s)" Exp.name ;
    let%map _, (_, out) =
      Exp.run_from_string_paths
        (file_type, compiler_input_fn)
        ~infile ~outfile
    in
    out)

module In = Asm_common.Input

let run output_format (input : In.t) =
  let o = In.output input in
  let infile = In.infile_raw input in
  let outfile = In.outfile_raw input in
  let target = In.target input in
  let file_type = In.file_type input in
  Or_error.Let_syntax.(
    let explain_cfg ~c_variables =
      ignore (c_variables : A.C_variables.Map.t option) ;
      Act_asm.Explainer.Config.make ?format:output_format ()
    in
    let compiler_input_fn = In.make_compiler_input input explain_cfg in
    A.Output.pv o "About to get and run the explain filter.@." ;
    let%map out =
      run_with_input_fn o file_type target compiler_input_fn infile outfile
    in
    A.Output.pw o "@[%a@]@." Act_asm.Job.Output.warn out ;
    print_symbol_map (Act_asm.Job.Output.symbol_map out))

let command =
  Command.basic ~summary:"explains act's understanding of an assembly file"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard_asm.get
      and output_format =
        Act_asm.Explainer.Config.Format.(
          choose_one
            [ map
                ~f:(fun flag -> Option.some_if flag (Some Detailed))
                (flag "detailed" no_arg
                   ~doc:"Print a detailed (but long-winded) explanation")
            ; map
                ~f:(fun flag -> Option.some_if flag (Some Assembly))
                (flag "as-assembly" no_arg
                   ~doc:"Print explanation as lightly annotated assembly")
            ]
            ~if_nothing_chosen:(`Default_to None))
      in
      fun () ->
        Asm_common.lift_command standard_args ~f:(run output_format)
          ~default_passes:Act_sanitiser.Pass_group.explain)
