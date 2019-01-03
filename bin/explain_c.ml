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

module Normal_C : Filter.S with type aux_i = unit and type aux_o = unit =
  Filter.Make (struct
    type aux_i = unit
    type aux_o = unit

    let run () is ic _ oc =
      Or_error.(
        C.Frontend.Normal.load_from_ic ~path:(Io.In_source.to_string is) ic
        >>| Fmt.pf (Format.formatter_of_out_channel oc) "%a@." C.Ast.Translation_unit.pp
      )
    ;;
  end)

module Litmus : Filter.S with type aux_i = unit and type aux_o = unit =
  Filter.Make (struct
    type aux_i = unit
    type aux_o = unit

    let run () is ic _ oc =
      Or_error.(
        C.Frontend.Litmus.load_from_ic ~path:(Io.In_source.to_string is) ic
        >>= C.Ast.Litmus.validate
        >>| Fmt.pf (Format.formatter_of_out_channel oc) "%a@." C.Ast.Litmus.pp
      )
    ;;
  end)

let c_module is_c
  : (module Filter.S with type aux_i = unit and type aux_o = unit) =
  if is_c then (module Normal_C) else (module Litmus)
;;

let run file_type ~(infile_raw : string option) ~(outfile_raw : string option) _o cfg =
  let open Or_error.Let_syntax in
  let%bind infile  = Io.fpath_of_string_option infile_raw in
  let%bind outfile = Io.fpath_of_string_option outfile_raw in
  let      is_c    = Common.decide_if_c infile file_type in
  let      cpp_cfg =
    Option.value (Lib.Config.M.cpp cfg) ~default:(Lib.Cpp.Config.default ())
  in
  let (module M)   = c_module is_c in
  let module Cpp_M = Lib.Cpp.Chain_filter (M) in
  let%map (_, ()) =
    Cpp_M.run_from_fpaths (cpp_cfg, ()) ~infile ~outfile
  in ()
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
