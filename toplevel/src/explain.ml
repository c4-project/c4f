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
open Lib
module A = Act_common

let print_symbol_map = function
  | [] ->
      ()
  | map ->
      Fmt.(
        pr "@[<v>@,Symbol map:@,@,%a@]@."
          (list ~sep:sp (fun f (k, v) -> pf f "@[<hv>%s@ ->@ %s@]" k v))
          map)

let run file_type compiler_id_or_arch output_format
    (c_globals : string list option) (c_locals : string list option)
    (args : Args.Standard_with_files.t) o cfg =
  let open Or_error.Let_syntax in
  let%bind target = Common.get_target cfg compiler_id_or_arch in
  let passes =
    Config.Act.sanitiser_passes cfg ~default:Config.Sanitiser_pass.explain
  in
  let explain_cfg ~c_variables =
    ignore (c_variables : A.C_variables.Map.t option) ;
    Asm_job.Explain_config.make ?format:output_format ()
  in
  let%bind (module Exp) = Common.explain_pipeline target in
  let%bind user_cvars = Common.collect_cvars ?c_globals ?c_locals () in
  let compiler_input_fn =
    Common.make_compiler_input o file_type user_cvars explain_cfg passes
  in
  let%map _, (_, out) =
    Exp.run_from_string_paths
      (file_type, compiler_input_fn)
      ~infile:(Args.Standard_with_files.infile_raw args)
      ~outfile:(Args.Standard_with_files.outfile_raw args)
  in
  A.Output.pw o "@[%a@]@." Asm_job.Output.warn out ;
  print_symbol_map (Asm_job.Output.symbol_map out)

let command =
  Command.basic ~summary:"explains act's understanding of an assembly file"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard_with_files.get
      and sanitiser_passes = Args.sanitiser_passes
      and compiler_id_or_arch = Args.compiler_id_or_arch
      and c_globals = Args.c_globals
      and c_locals = Args.c_locals
      and output_format =
        Asm_job.Explain_config.Format.(
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
      and file_type = Args.file_type in
      fun () ->
        Common.lift_command_with_files standard_args ?sanitiser_passes
          ~with_compiler_tests:false
          ~f:
            (run file_type compiler_id_or_arch output_format c_globals
               c_locals))
