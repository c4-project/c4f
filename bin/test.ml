(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
   LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core
open Lib
open Utils

let report_spec_errors o = function
  | [] -> ()
  | es ->
    Format.fprintf o.Output.wf
      "@[<v>Some of the specified compilers don't seem to be valid:@,@,%a@]@."
      (Format.pp_print_list Error.pp ~pp_sep:Format.pp_print_cut)
      es
;;

let make_tester_config
    ~(out_root_raw : string)
    ~(input_mode : Tester.Input_mode.t)
    o cfg :
  Tester.Run_config.t Or_error.t =
  let open Or_error.Let_syntax in
  let%bind output_root = Io.fpath_of_string out_root_raw
  in
  let specs = Config.M.compilers cfg in
  report_spec_errors o
    (List.filter_map ~f:snd (Config.M.disabled_compilers cfg));
  let compilers =
    specs
    |> Compiler.Spec.Set.map ~f:(Compiler.Spec.With_id.id)
    |> Id.Set.of_list
  in
  Tester.Run_config.make
    ~output_root
    ~compilers
    ~input_mode
;;

let make_tester o cfg timing_mode : (module Tester.Instance.S) =
  (module
    Tester.Instance.Make
      (struct
        module T = (val Utils.Timing.Mode.to_module timing_mode)
        module Resolve_compiler = Language_support.Resolve_compiler

        let o = o
        let compilers = Config.M.compilers cfg
        let sanitiser_passes = Config.M.sanitiser_passes cfg
            ~default:Sanitiser_pass.standard
        let herd_cfg = Config.M.herd cfg
        let asm_runner_from_spec =
          Fn.compose
            Language_support.asm_runner_from_arch
            Compiler.Spec.With_id.emits
      end)
  )
;;

let print_table = Fmt.pr "@[<v>%a@]@." Tabulator.pp

let cook_memalloy in_root_raw =
  let open Or_error.Let_syntax in
  let%bind input_root = Io.fpath_of_string in_root_raw in
  Tester.Input_mode.memalloy ~input_root
;;

let cook_delitmus files_raw =
  let open Or_error.Let_syntax in
  let%bind files =
    files_raw
    |> List.map ~f:Io.fpath_of_string
    |> Or_error.combine_errors
  in
  Tester.Input_mode.litmus_only ~files
;;

let cook_input_mode = function
  | `Memalloy in_root_raw -> cook_memalloy in_root_raw
  | `Delitmus files_raw -> cook_delitmus files_raw
;;

let run
    (should_time : bool)
    (input_mode_raw : [< `Delitmus of string list | `Memalloy of string ])
    (out_root_raw : string)
    (o : Output.t)
    (cfg : Config.M.t)
    : unit Or_error.t =
  let open Or_error.Let_syntax in
  let%bind input_mode = cook_input_mode input_mode_raw in
  let%bind tester_cfg =
    make_tester_config ~input_mode ~out_root_raw o cfg
  in
  let timing_mode =
    Timing.Mode.(if should_time then Enabled else Disabled)
  in
  let (module T) = make_tester o cfg timing_mode in
  let%bind analysis = T.run tester_cfg in
  let%map table = Tester.Analysis.to_table analysis in
  print_table table
;;

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"runs automatic testing over a memalloy output directory"
    [%map_open
      let standard_args = Args.Standard.get
      and out_root_raw =
        flag_optional_with_default_doc "output"
          ~default:Filename.current_dir_name
          string [%sexp_of: string]
          ~doc:"PATH the path under which output directories will be created"
      and time =
        flag "time"
          no_arg
          ~doc:"if given, measure and report times"
      and input_mode_raw =
          choose_one
            [ (map ~f:(Option.map ~f:(fun x -> `Memalloy x))
                 (flag "memalloy" (optional Filename.arg_type)
                    ~doc:"DIRECTORY read input from a Memalloy run at this directory"
                 )
              )
            ; (map ~f:(Option.map ~f:(fun x -> `Delitmus x))
                 (anon (maybe (non_empty_sequence_as_list ("TEST_FILE" %: Filename.arg_type))))
              )
            ]
            ~if_nothing_chosen:`Raise
      and sanitiser_passes = Args.sanitiser_passes
      and compiler_predicate = Args.compiler_predicate
      and machine_predicate = Args.machine_predicate
      in
      fun () ->
        Common.lift_command standard_args
          ?compiler_predicate
          ?machine_predicate
          ?sanitiser_passes
          ~with_compiler_tests:true
          ~f:(fun _args -> run time input_mode_raw out_root_raw)
    ]
;;
