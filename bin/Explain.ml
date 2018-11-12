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

let print_symbol_map = function
  | [] -> ()
  | map ->
    Format.printf "@[<v>@,Symbol map:@,@,%a@]@."
      (Format.pp_print_list
         ~pp_sep:Format.pp_print_space
         (fun f (k, v) -> Format.fprintf f "@[<hv>%s@ ->@ %s@]" k v))
      map
;;

let run id symbols sanitise ~infile ~outfile o cfg =
  let open Or_error.Let_syntax in
  let passes = Sanitiser_pass.(
      if sanitise then explain else Set.empty
    )
  in
  let%bind spec =
    Compiler.Spec.Set.get (Config.M.compilers cfg) id
  in
  let cspec =
    Compiler.Spec.With_id.create ~id ~spec
  in
  let%bind runner =
    Language_support.asm_runner_from_spec cspec
  in
  let module Runner = (val runner) in
  Io.(
    let input =
      { Asm_job.inp = In_source.of_option infile
      ; outp = Out_sink.of_option outfile
      ; passes
      ; symbols
      }
    in
    let%map out = Runner.explain input in
    Asm_job.warn out o.Output.wf;
    print_symbol_map (Asm_job.symbol_map out)
  )
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"explains act's understanding of an assembly file"
    [%map_open
      let standard_args = Standard_args.get
      and sanitise =
        flag "sanitise"
          no_arg
          ~doc: "if true, do basic sanitisation on the assembly first"
      and compiler_id = Standard_args.Other.compiler_id_anon
      and symbols =
        flag_optional_with_default_doc "track"
          ~default:[]
          (Arg_type.comma_separated
             ~unique_values:true
             ~strip_whitespace:true
             string)
          [%sexp_of: string list]
          ~doc: "SYMBOLS comma-separated list of symbols to track"
      and outfile =
        flag "output"
          (optional file)
          ~doc: "FILE the explanation output file (default: stdout)"
      and infile =
        anon (maybe ("FILE" %: file))
      in
      fun () ->
        Common.lift_command standard_args
          ~with_compiler_tests:false
          ~f:(run compiler_id symbols sanitise ~infile ~outfile)
    ]
;;
