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

let run_herd cfg target (path : Fpath.t) (sink : Io.Out_sink.t) _chan
  : unit Or_error.t =
  let open Result.Let_syntax in
  let%bind herd = make_herd cfg in
  let arch = Herd.Assembly (Common.arch_of_target target) in
  Herd.run herd arch ~path ~sink
;;

let run file_type use_herd compiler_id_or_emits
    c_symbols
    ~(infile_raw : string option)
    ~(outfile_raw : string option) o cfg =
  Common.warn_if_not_tracking_symbols o c_symbols;
  let open Result.Let_syntax in
  let%bind target = Common.get_target cfg compiler_id_or_emits in
  let passes =
    Config.M.sanitiser_passes cfg ~default:Sanitiser_pass.standard
  in
  let (module Lit)
    = Common.litmusify_filter o passes c_symbols target
  in
  let%bind (module Comp_lit) =
    Common.maybe_run_compiler (module Lit) target file_type
  in
  let ( module Flt : Filter.S with type aux_i = ((unit * unit) * unit)
                               and type aux_o = ((unit option * (string, string) List.Assoc.t) * unit option)
      ) =
    (module
      Filter.Chain_conditional_second (struct
        let condition _ _ = use_herd
        module First = Comp_lit
        module Second = Filter.Make_in_file_only (struct
            type aux_i = unit
            type aux_o = unit
            let run () = run_herd cfg target
          end)
      end))
  in
  let%map _ = Flt.run_from_string_paths (((),()),()) ~infile:infile_raw ~outfile:outfile_raw in
  ()
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
      and outfile_raw =
        flag "output"
          (optional file)
          ~doc: "FILE the litmus output file (default: stdout)"
      and infile_raw = anon (maybe ("FILE" %: file)) in
      fun () ->
        Common.lift_command standard_args
          ?sanitiser_passes
          ~with_compiler_tests:false
          ~f:(run
                file_type use_herd compiler_id_or_arch c_symbols
                ~infile_raw ~outfile_raw)
    ]
