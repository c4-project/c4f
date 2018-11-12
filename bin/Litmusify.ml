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

let make_herd cfg =
  let open Or_error.Let_syntax in
  let%bind herd_cfg =
    Result.of_option (Config.M.herd cfg)
      ~error:(Error.of_string
                "No Herd stanza in configuration"
             )
  in
  Herd.create ~config:herd_cfg
;;

let temp_file = Filename.temp_file "act"

let asm_file is_c maybe_infile =
  if is_c then Some (temp_file "s") else maybe_infile
;;

let lit_file use_herd maybe_outfile =
  if use_herd then Some (temp_file "litmus") else maybe_outfile
;;

let run_compiler o cspec c_file asm_file =
  let open Result.Let_syntax in
  let%bind infile =
    Result.of_option c_file
      ~error:(Error.of_string "Can't read in C from stdin")
  in
  let%bind outfile =
    Result.of_option asm_file
      ~error:(Error.of_string "Can't output compiler result to stdout")
  in
  let name = Filename.basename infile in
  let%bind c = Language_support.compiler_from_spec cspec in
  let%map _ =
    Common.compile_with_compiler c o ~name ~infile ~outfile
      (Compiler.Spec.With_id.id cspec)
  in
  ()
;;

let run_litmusify o cspec asm_file lit_file =
  let source = Io.In_source.of_option asm_file in
  let sink = Io.Out_sink.of_option lit_file in
  Common.litmusify o source sink [] cspec
;;

let run_herd cfg cspec lit_file outfile =
  let open Result.Let_syntax in
  let%bind path =
    Result.of_option lit_file
      ~error:(Error.of_string "Can't read in litmus from stdin")
  in
  let sink = Io.Out_sink.of_option outfile in
  let%bind herd = make_herd cfg in
  let arch = Herd.Assembly (Compiler.Spec.With_id.emits cspec) in
  Herd.run herd arch ~path ~sink
;;

let decide_if_c infile = function
  | `C -> true
  | `Assembly -> false
  | `Infer ->
    Option.exists infile
      ~f:(My_filename.has_extension ~ext:"c")
;;

let run file_type use_herd id ~infile ~outfile o cfg =
  let open Result.Let_syntax in
  let%bind spec = Compiler.Spec.Set.get (Config.M.compilers cfg) id in
  let cspec = Compiler.Spec.With_id.create ~id ~spec in

  let is_c = decide_if_c infile file_type in

  let asm_file = asm_file is_c infile in
  let lit_file = lit_file use_herd outfile in

  let%bind () =
    if is_c then run_compiler o cspec infile asm_file else return ()
  in
  let%bind _ = run_litmusify o cspec asm_file lit_file in
  if use_herd then run_herd cfg cspec lit_file outfile else return ()
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"converts an assembly file to a litmus test"
    [%map_open
      let standard_args = Standard_args.get
      and use_herd =
        flag "herd"
          no_arg
          ~doc: "if true, pipe results through herd"
      and file_type =
        choose_one
          [ (let%map c =
               flag "c"
                 no_arg
                 ~doc: "if given, assume input is C (and compile it)"
             in (Option.some_if c `C))
          ; (let%map asm =
               flag "asm"
                 no_arg
                 ~doc: "if given, assume input is assembly"
             in (Option.some_if asm `Assembly))
          ]
          ~if_nothing_chosen:(`Default_to `Infer)
      and compiler_id = Standard_args.Other.compiler_id_anon
      and outfile =
        flag "output"
          (optional file)
          ~doc: "FILE the litmus output file (default: stdout)"
      and infile =
        anon (maybe ("FILE" %: file))
      in
      fun () ->
        Common.lift_command standard_args
          ~with_compiler_tests:false
          ~f:(run file_type use_herd compiler_id ~infile ~outfile)
    ]
