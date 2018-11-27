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

let run file_type compiler_id_or_arch output_format c_symbols sanitise
    ~infile ~outfile o cfg =
  Common.warn_if_not_tracking_symbols o c_symbols;
  let open Or_error.Let_syntax in
  let passes = Sanitiser_pass.(
      if sanitise then explain else Set.empty
    )
  in
  let%bind target = Common.get_target cfg compiler_id_or_arch in
  let%bind asm_file =
    Common.maybe_run_compiler target file_type infile
  in
  let%bind (module Runner) = Common.runner_of_target target in
  Io.(
    let input =
      { Asm_job.inp = In_source.of_option asm_file
      ; outp = Out_sink.of_option outfile
      ; passes
      ; symbols = c_symbols
      }
    in
    let%map out = Runner.explain ?output_format input in
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
      and compiler_id_or_arch = Standard_args.Other.compiler_id_or_arch
      and c_symbols = Standard_args.Other.c_symbols
      and output_format =
        Asm_job.Explain_format.(
          choose_one
            [ map ~f:(fun flag -> Option.some_if flag (Some Detailed))
                (flag "detailed"
                   no_arg
                   ~doc: "Print a detailed (but long-winded) explanation")
            ; map ~f:(fun flag -> Option.some_if flag (Some Assembly))
                (flag "as-assembly"
                   no_arg
                   ~doc: "Print explanation as lightly annotated assembly")
            ]
            ~if_nothing_chosen:(`Default_to None)
        )
      and file_type = Standard_args.Other.file_type
      and outfile =
        flag "output"
          (optional file)
          ~doc: "FILE the explanation output file (default: stdout)"
      and infile = anon (maybe ("FILE" %: file)) in
      fun () ->
        Common.lift_command standard_args
          ~with_compiler_tests:false
          ~f:(run
                file_type
                compiler_id_or_arch
                output_format
                c_symbols sanitise
                ~infile ~outfile)
    ]
;;
