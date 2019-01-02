(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core
open Lib
open Utils

let compile spec ~c_file =
  let open Or_error.Let_syntax in
  let%bind maybe_outfile =
    Common.maybe_run_compiler (`Spec spec) `C (Some c_file)
  in
  Result.of_option maybe_outfile
    ~error:(Error.of_string "Internal error: expected an output file")
;;

let litmusify o passes spec ~asm_file =
  Common.litmusify o
    ~output_format:Asm_job.Litmus_format.Programs_only
    passes
    (Io.In_source.file asm_file)
    Io.Out_sink.stdout
    []
    spec
;;

let run_spec_on_file o passes spec ~c_file =
  Format.printf "@[<v>@,@[<h>##@ %a@]@,@,```@]@."
    Id.pp (Compiler.Spec.With_id.id spec);
  let open Or_error.Let_syntax in
  let%bind asm_file = compile spec ~c_file in
  let%map  _        = litmusify o passes (`Spec spec) ~asm_file in
  Format.printf "@[<h>```@]@."
;;

let run o cfg ~(c_file_raw : string) =
  let open Or_error.Let_syntax in
  let%bind c_file = Io.fpath_of_string c_file_raw in
  let specs = Config.M.compilers cfg in
  let passes =
    Config.M.sanitiser_passes cfg ~default:Sanitiser_pass.standard
  in
  Fmt.pr "@[<h>#@ %a@]@." Fpath.pp c_file;
  Or_error.combine_errors_unit
    (Compiler.Spec.Set.map specs
       ~f:(run_spec_on_file o passes ~c_file)
    )
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"displays the litmus output for each compiler over a C file"
    [%map_open
      let standard_args = Standard_args.get
      and sanitiser_passes = Standard_args.Other.sanitiser_passes
      and compiler_predicate = Standard_args.Other.compiler_predicate
      and machine_predicate = Standard_args.Other.machine_predicate
      and c_file_raw = anon ("FILE" %: file)
      in
      fun () ->
        Common.lift_command standard_args
          ?compiler_predicate
          ?machine_predicate
          ?sanitiser_passes
          ~with_compiler_tests:true
          ~f:(run ~c_file_raw)
    ]
;;
