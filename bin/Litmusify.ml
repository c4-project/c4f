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

let run_litmusify o passes target c_symbols
    asm_file lit_file =
  let source = Io.In_source.of_option asm_file in
  let sink = Io.Out_sink.of_option lit_file in
  Common.litmusify o passes source sink c_symbols target
;;

let run_herd cfg target lit_file outfile =
  let open Result.Let_syntax in
  let%bind path =
    Result.of_option lit_file
      ~error:(Error.of_string "Can't read in litmus from stdin")
  in
  let sink = Io.Out_sink.of_option outfile in
  let%bind herd = make_herd cfg in
  let arch = Herd.Assembly (Common.arch_of_target target) in
  let%map (_, ()) = Herd.run herd arch ~path ~sink in ()
;;

let lit_file use_herd maybe_outfile =
  if use_herd then Some (Common.temp_file "litmus") else maybe_outfile
;;

let run file_type use_herd compiler_id_or_emits
    c_symbols
    ~infile ~outfile o cfg =
  Common.warn_if_not_tracking_symbols o c_symbols;
  let open Result.Let_syntax in
  let%bind target = Common.get_target cfg compiler_id_or_emits in
  let%bind asm_file =
    Common.maybe_run_compiler target file_type infile
  in
  let lit_file = lit_file use_herd outfile in
  let passes =
    Config.M.sanitiser_passes cfg ~default:Sanitiser_pass.standard
  in
  let%bind _ = run_litmusify o passes target c_symbols asm_file lit_file in
  Travesty.T_or_error.when_m use_herd
    ~f:(fun () -> run_herd cfg target lit_file outfile)
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"converts an assembly file to a litmus test"
    [%map_open
      let standard_args = Standard_args.get
      and sanitiser_passes = Standard_args.Other.sanitiser_passes
      and use_herd =
        flag "herd"
          no_arg
          ~doc: "if true, pipe results through herd"
      and file_type = Standard_args.Other.file_type
      and compiler_id_or_arch = Standard_args.Other.compiler_id_or_arch
      and c_symbols = Standard_args.Other.c_symbols
      and outfile =
        flag "output"
          (optional file)
          ~doc: "FILE the litmus output file (default: stdout)"
      and infile = anon (maybe ("FILE" %: file)) in
      fun () ->
        Common.lift_command standard_args
          ?sanitiser_passes
          ~with_compiler_tests:false
          ~f:(run
                file_type use_herd compiler_id_or_arch c_symbols
                ~infile ~outfile)
    ]
