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

let compile o spec ~c_file =
  let open Or_error.Let_syntax in
  let infile = c_file in
  let outfile = Filename.temp_file "act" "s" in
  let%bind c = Language_support.compiler_from_spec spec in
  let%map _ =
    Common.compile_with_compiler c o (Compiler.Spec.With_id.id spec)
      ~name:(Filename.basename infile)
      ~infile
      ~outfile in
  outfile
;;

let litmusify o spec ~asm_file =
  Common.litmusify o
    ~programs_only:true
    (`File asm_file)
    `Stdout
    []
    spec
;;

let run_spec_on_file o spec ~c_file =
  Format.printf "@[<v>@,@[<h>##@ %a@]@,@,```@]@."
    Id.pp (Compiler.Spec.With_id.id spec);
  let open Or_error.Let_syntax in
  let%bind asm_file = compile o spec ~c_file in
  let%map  _        = litmusify o (`Spec spec) ~asm_file in
  Format.printf "@[<h>```@]@."
;;

let run_all_specs_on_file o specs ~c_file =
  Format.printf "@[<h>#@ %s@]@." c_file;
  Or_error.combine_errors_unit
    (Compiler.Spec.Set.map specs
       ~f:(run_spec_on_file o ~c_file)
    )
;;

let run o cfg ~c_file =
  let specs = Config.M.compilers cfg in
  run_all_specs_on_file o specs ~c_file
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"displays the litmus output for each compiler over a C file"
    [%map_open
      let standard_args = Standard_args.get
      and compiler_predicate = Standard_args.Other.compiler_predicate
      and machine_predicate = Standard_args.Other.machine_predicate
      and c_file = anon ("C_FILE" %: file)
      in
      fun () ->
        Common.lift_command standard_args
          ?compiler_predicate
          ?machine_predicate
          ~with_compiler_tests:true
          ~f:(run ~c_file)
    ]
;;
