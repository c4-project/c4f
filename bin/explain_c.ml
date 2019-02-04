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

let run_delitmus (args : Args.Standard_with_files.t) _o _cfg =
  let open Or_error.Let_syntax in
  let%map _ =
    C.Filters.Litmus.run_from_string_paths
      C.Filters.Delitmus
      ~infile:(Args.Standard_with_files.infile_raw args)
      ~outfile:(Args.Standard_with_files.outfile_raw args)
  in ()
;;

let delitmus_command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"converts a C litmus test to a normal C file"
    [%map_open
      let standard_args = Args.Standard_with_files.get in
      fun () ->
        Common.lift_command_with_files standard_args
          ~with_compiler_tests:false
          ~f:run_delitmus
    ]
;;

let run
    (file_type : [ `C | `Litmus | `Infer ])
    (output_mode : [ `All | `Vars ])
    (args : Args.Standard_with_files.t)
    _o cfg =
  let open Or_error.Let_syntax in
  let%bind infile  = Args.Standard_with_files.infile_source args in
  let%bind outfile = Args.Standard_with_files.outfile_sink args in
  let      is_c    = Lib.File_type.is_c infile file_type in
  let      cpp_cfg =
    Option.value (Lib.Config.M.cpp cfg) ~default:(Lib.Cpp.Config.default ())
  in
  let (module M)   = C.Filters.c_module is_c in
  let module Cpp_M = Lib.Cpp.Chain_filter (M) in
  let%map _ =
    Cpp_M.run (cpp_cfg, Print output_mode) infile outfile
  in ()
;;

let explain_command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"explains act's understanding of a C file"
    [%map_open
      let standard_args = Args.Standard_with_files.get
      and file_type =
        choose_one
          [ Args.flag_to_enum_choice `C "c"
              ~doc:"assume input is raw C"
          ; Args.flag_to_enum_choice `Litmus "litmus"
              ~doc:"assume input is a C litmus test"
          ]
          ~if_nothing_chosen:(`Default_to `Infer)
      and output_mode =
        choose_one
          [ Args.flag_to_enum_choice `All "dump-input"
              ~doc:"pretty-print the input back onto stdout (the default)"
          ; Args.flag_to_enum_choice `Vars "dump-vars"
              ~doc:"emit the names of all variables found in the input"
          ]
          ~if_nothing_chosen:(`Default_to `All)
      in
      fun () ->
        Common.lift_command_with_files standard_args
          ~with_compiler_tests:false
          ~f:(run file_type output_mode)
    ]
;;

let command : Command.t =
  Command.group
    ~summary:"Commands for dealing with C files"
    [ "delitmus", delitmus_command
    ; "explain", explain_command
    ]
;;
