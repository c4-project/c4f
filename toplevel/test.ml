(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core
open Lib
open Utils

let report_spec_errors (o : Output.t) = function
  | [] ->
      ()
  | es ->
      Output.pw o
        "@[<v>Some of the specified compilers don't seem to be \
         valid:@,@,%a@]@."
        Fmt.(list Error.pp ~sep:cut)
        es

let make_tester_config ~(out_root_raw : string)
    ~(input_mode : Tester.Input_mode.t) o cfg :
    Tester.Run_config.t Or_error.t =
  let open Or_error.Let_syntax in
  let%bind output_root_dir = Io.fpath_of_string out_root_raw in
  let specs = Config.Act.compilers cfg in
  report_spec_errors o
    (List.filter_map ~f:snd (Config.Act.disabled_compilers cfg)) ;
  let compilers =
    specs
    |> Config.Compiler.Spec.Set.map ~f:Config.Compiler.Spec.With_id.id
    |> Config.Id.Set.of_list
  in
  let%map pathset =
    Tester.Pathset.Run.make_and_mkdirs {output_root_dir; input_mode}
  in
  Tester.Run_config.make ~pathset ~compilers

let make_tester o cfg timing_mode =
  ( module Tester.Instance.Make (struct
    module T = (val Utils.Timing.Mode.to_module timing_mode)

    module Resolve_compiler = Language_support.Resolve_compiler

    let o = o

    let compilers = Config.Act.compilers cfg

    let sanitiser_passes =
      Config.Act.sanitiser_passes cfg
        ~default:Config.Sanitiser_pass.standard

    let herd_cfg = Config.Act.herd cfg

    let asm_runner_from_spec =
      Fn.compose Language_support.asm_runner_from_arch
        Config.Compiler.Spec.With_id.emits
  end)
  : Tester.Instance.S )

let print_table t = Tabulator.print t ; Stdio.print_endline ""

let cook_memalloy in_root_raw =
  let open Or_error.Let_syntax in
  let%bind input_root = Io.fpath_of_string in_root_raw in
  Tester.Input_mode.memalloy ~input_root

let cook_delitmus files_raw =
  let open Or_error.Let_syntax in
  let%bind files =
    files_raw |> List.map ~f:Io.fpath_of_string |> Or_error.combine_errors
  in
  Tester.Input_mode.litmus_only ~files

let cook_input_mode = function
  | `Memalloy in_root_raw ->
      cook_memalloy in_root_raw
  | `Delitmus files_raw ->
      cook_delitmus files_raw

let output_main_table (analysis : Tester.Analysis.t) : unit Or_error.t =
  (* TODO(@MattWindsor): wire this up *)
  let a =
    {Tester.Analysis_table.Interest_level.data= analysis; level= All}
  in
  Or_error.(a |> Tester.Analysis_table.On_files.to_table >>| print_table)

let non_empty : 'a list -> 'a list option = function
  | [] ->
      None
  | xs ->
      Some xs

let pp_deviation_bucket (bucket_name : string) :
    Sim_output.State.t list Fmt.t =
  Fmt.(
    using non_empty
      (option
         ~none:
           (styled `Yellow (const (fmt "No states in %s only.") bucket_name))
         (vbox ~indent:2
            (prefix
               (suffix sp
                  (styled `Red (const (fmt "In %s only:") bucket_name)))
               (vbox
                  (list ~sep:sp
                     (using [%sexp_of: Sim_output.State.t] Sexp.pp_hum)))))))

let to_deviation_lists (ord : Sim_diff.Order.t) :
    Sim_output.State.t list * Sim_output.State.t list =
  let l = ord |> Sim_diff.Order.in_left_only |> Set.to_list in
  let r = ord |> Sim_diff.Order.in_right_only |> Set.to_list in
  (l, r)

let pp_deviation : Sim_diff.Order.t Fmt.t =
  Fmt.(
    using to_deviation_lists
      (pair ~sep:sp (pp_deviation_bucket "C") (pp_deviation_bucket "asm")))

let pp_deviation_row : Sim_diff.Order.t Tester.Analysis_table.Row.t Fmt.t =
  Fmt.(
    vbox ~indent:2
      (using
         Tester.Analysis_table.Row.(
           fun x -> (((compiler_id x, machine_id x), filename x), analysis x))
         (pair ~sep:sp
            (hbox
               (pair ~sep:(unit ":")
                  (pair ~sep:(unit "@") Config.Id.pp Config.Id.pp)
                  string))
            pp_deviation)))

let output_deviations (analysis : Tester.Analysis.t) : unit =
  match Tester.Analysis_table.On_deviations.rows analysis with
  | [] ->
      ()
  | rs ->
      Fmt.(
        pr "@[<v 2>The following files had deviations:@,%a@]@."
          (list ~sep:sp pp_deviation_row))
        rs

let output_analysis (analysis : Tester.Analysis.t) : unit Or_error.t =
  let open Or_error.Let_syntax in
  let%map () = output_main_table analysis in
  output_deviations analysis

let run (should_time : bool)
    (input_mode_raw : [< `Delitmus of string list | `Memalloy of string])
    (out_root_raw : string) (o : Output.t) (cfg : Config.Act.t) :
    unit Or_error.t =
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
  output_analysis analysis

let command =
  Command.basic
    ~summary:"runs automatic testing over a memalloy output directory"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard.get
      and out_root_raw =
        flag_optional_with_default_doc "output"
          ~default:Filename.current_dir_name string [%sexp_of: string]
          ~doc:
            "PATH the path under which output directories will be created"
      and time =
        flag "time" no_arg ~doc:"if given, measure and report times"
      and input_mode_raw =
        choose_one
          [ map
              ~f:(Option.map ~f:(fun x -> `Memalloy x))
              (flag "memalloy"
                 (optional Filename.arg_type)
                 ~doc:
                   "DIRECTORY read input from a Memalloy run at this \
                    directory")
          ; map
              ~f:(Option.map ~f:(fun x -> `Delitmus x))
              (anon
                 (maybe
                    (non_empty_sequence_as_list
                       ("TEST_FILE" %: Filename.arg_type)))) ]
          ~if_nothing_chosen:`Raise
      and sanitiser_passes = Args.sanitiser_passes
      and compiler_predicate = Args.compiler_predicate
      and machine_predicate = Args.machine_predicate in
      fun () ->
        Common.lift_command standard_args ?compiler_predicate
          ?machine_predicate ?sanitiser_passes ~with_compiler_tests:true
          ~f:(fun _args -> run time input_mode_raw out_root_raw))
