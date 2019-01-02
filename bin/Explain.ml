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

let run file_type compiler_id_or_arch output_format c_symbols
    ~(infile_raw : string option) ~(outfile_raw : string option) o cfg =
  Common.warn_if_not_tracking_symbols o c_symbols;
  let open Or_error.Let_syntax in
  let%bind outfile = Io.fpath_of_string_option outfile_raw
  and      infile  = Io.fpath_of_string_option infile_raw
  in
  let passes =
    Config.M.sanitiser_passes cfg ~default:Sanitiser_pass.Set.empty
  in
  let%bind target = Common.get_target cfg compiler_id_or_arch in
  let%bind asm_file =
    Common.maybe_run_compiler target file_type infile
  in
  let inp = Io.In_source.of_fpath_opt asm_file in
  let%bind (module Runner) = Common.runner_of_target target in
  Io.(
    let input =
      { Asm_job.inp
      ; outp = Out_sink.of_fpath_opt outfile
      ; passes
      ; symbols = c_symbols
      }
    in
    let%map (_, out) = Runner.explain ?output_format input in
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
      and sanitiser_passes = Standard_args.Other.sanitiser_passes
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
      and outfile_raw =
        flag "output"
          (optional file)
          ~doc: "FILE the explanation output file (default: stdout)"
      and infile_raw = anon (maybe ("FILE" %: file)) in
      fun () ->
        Common.lift_command standard_args
          ?sanitiser_passes
          ~with_compiler_tests:false
          ~f:(run
                file_type
                compiler_id_or_arch
                output_format
                c_symbols
                ~infile_raw ~outfile_raw)
    ]
;;
