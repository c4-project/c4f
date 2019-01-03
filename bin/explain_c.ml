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
open Utils

let run_c is os =
  Or_error.(
    C.Frontend.Normal.load_from_isrc is
    >>= fun ast ->
    Io.Out_sink.with_output os
      ~f:(fun _ oc ->
          Fmt.pf (Format.formatter_of_out_channel oc) "%a@." C.Ast.Translation_unit.pp ast;
          Result.ok_unit
        )
  )
;;

let run_litmus is os =
  Or_error.(
    C.Frontend.Litmus.load_from_isrc is
    >>= fun ast ->
    Io.Out_sink.with_output os
      ~f:(fun _ oc ->
          ast |> C.Ast.Litmus.validate >>|
          Fmt.pf (Format.formatter_of_out_channel oc) "%a@." C.Ast.Litmus.pp
        )
  )
;;

let run file_type ~infile_raw ~outfile_raw _o _cfg =
  let open Or_error.Let_syntax in
  let%bind infile = Io.fpath_of_string_option infile_raw in
  let      is     = Io.In_source.of_fpath_opt infile in
  let%bind os     = Io.Out_sink.of_string_opt outfile_raw in
  let      is_c   = Common.decide_if_c infile file_type in
  (if is_c then run_c else run_litmus) is os
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"explains act's understanding of a C file"
    [%map_open
      let standard_args = Standard_args.get
      and outfile_raw =
        flag "output"
          (optional file)
          ~doc: "FILE the explanation output file (default: stdout)"
      and file_type =
        choose_one
          [ Standard_args.Other.flag_to_enum_choice `C "c"
              ~doc:"if given, assume input is raw C"
          ; Standard_args.Other.flag_to_enum_choice `Litmus "litmus"
              ~doc:"if given, assume input is a C litmus test"
          ]
          ~if_nothing_chosen:(`Default_to `Infer)
      and infile_raw = anon (maybe ("FILE" %: file)) in
      fun () ->
        Common.lift_command standard_args
          ~with_compiler_tests:false
          ~f:(run file_type ~infile_raw ~outfile_raw)
    ]
;;
