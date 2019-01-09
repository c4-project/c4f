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

let run_delitmus ~(infile_raw : string option) ~(outfile_raw : string option) _o _cfg =
  C.Filters.Litmus.run_from_string_paths
    C.Filters.Delitmus
    ~infile:infile_raw
    ~outfile:outfile_raw
;;

let delitmus_command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"converts a C litmus test to a normal C file"
    [%map_open
      let standard_args = Standard_args.get
      and outfile_raw =
        flag "output"
          (optional file)
          ~doc: "FILE the output file (default: stdout)"
      and infile_raw = anon (maybe ("FILE" %: file)) in
      fun () ->
        Common.lift_command standard_args
          ~with_compiler_tests:false
          ~f:(run_delitmus ~infile_raw ~outfile_raw)
    ]
;;

let run file_type ~(infile_raw : string option) ~(outfile_raw : string option) _o cfg =
  let open Or_error.Let_syntax in
  let%bind infile  = Io.In_source.of_string_opt infile_raw in
  let%bind outfile = Io.Out_sink.of_string_opt outfile_raw in
  let      is_c    = Common.is_c infile file_type in
  let      cpp_cfg =
    Option.value (Lib.Config.M.cpp cfg) ~default:(Lib.Cpp.Config.default ())
  in
  let (module M)   = C.Filters.c_module is_c in
  let module Cpp_M = Lib.Cpp.Chain_filter (M) in
  let%map (_, ()) =
    Cpp_M.run (cpp_cfg, Print) infile outfile
  in ()
;;

let explain_command : Command.t =
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

let command : Command.t =
  Command.group
    ~summary:"Commands for dealing with C files"
    [ "delitmus", delitmus_command
    ; "explain", explain_command
    ]
;;
