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

let run_machine
    o
    (should_time : bool)
    (cfg : Config.M.t)
    (tester_cfg : Tester_config.t)
    (mach_id, specs) =
  let open Or_error.Let_syntax in

  let timing_cfg = if should_time then `Enabled else `Disabled in

  let module TM = Tester.Make_machine (struct
      module T = (val Timing.make_module timing_cfg)
      let o = o
      let sanitiser_passes =
        Config.M.sanitiser_passes cfg ~default:Sanitiser_pass.standard
      ;;
      let herd_cfg = Config.M.herd cfg

      let compilers = specs

      module Resolve_compiler = Language_support.Resolve_compiler

      let asm_runner_from_spec spec =
        Language_support.asm_runner_from_arch
          (Compiler.Spec.With_id.emits spec)
    end) in
  let%map analysis = TM.run tester_cfg in
  (mach_id, analysis)
;;

let check_files_exist (c_path : Fpath.t) (c_files : Fpath.t list) =
  if List.is_empty c_files
  then Or_error.error_s
      [%message "Expected at least one file." ~path:(Fpath.to_string c_path)]
  else Result.ok_unit
  (* TODO(@MattWindsor91): actually test the files go somewhere? *)
;;

let report_spec_errors o = function
  | [] -> ()
  | es ->
    Format.fprintf o.Output.wf
      "@[<v>Some of the specified compilers don't seem to be valid:@,@,%a@]@."
      (Format.pp_print_list Error.pp ~pp_sep:Format.pp_print_cut)
      es
;;

let group_specs_by_machine specs =
  specs
  |> Compiler.Spec.Set.group
    ~f:(fun spec ->
        Machine.Spec.With_id.id
          (Compiler.Spec.With_id.machine spec)
      )
  |> Id.Map.to_alist
;;

let find_memalloy_c_filenames (in_root : Fpath.t)
  : string list Or_error.t =
  let open Or_error.Let_syntax in
  let c_path = Fpath.(in_root / "C") in
  let%bind c_files = Io.Dir.get_files c_path ~ext:"c" in
  let%map () = check_files_exist c_path c_files in
  List.map ~f:Io.filename_no_ext c_files
;;

let find_delitmusify_litmus_filenames (in_root : Fpath.t)
  : string list Or_error.t =
  let open Or_error.Let_syntax in
  let%bind lit_files = Io.Dir.get_files in_root ~ext:"litmus" in
  let%map () = check_files_exist in_root lit_files in
  List.map ~f:Io.filename_no_ext lit_files
;;

let find_filenames : Tester_config.C_litmus_mode.t -> Fpath.t -> string list Or_error.t =
  function
  | Tester_config.C_litmus_mode.Memalloy ->
    find_memalloy_c_filenames
  | Tester_config.C_litmus_mode.Delitmusify ->
    find_delitmusify_litmus_filenames
;;

let make_tester_config ~(in_root_raw : string) ~(out_root_raw : string) o cfg :
  Tester_config.t Or_error.t =
  let open Or_error.Let_syntax in
  (* TODO(@MattWindsor91): wire this up somehow *)
  let c_litmus_mode = Tester_config.C_litmus_mode.Memalloy in
  let%bind in_root  = Io.fpath_of_string in_root_raw
  and      out_root = Io.fpath_of_string out_root_raw
  in
  let%bind fnames = find_filenames c_litmus_mode in_root in
  let specs = Config.M.compilers cfg in
  report_spec_errors o
    (List.filter_map ~f:snd (Config.M.disabled_compilers cfg));
  (* TODO(@MattWindsor91): wire this up somehow *)
  let compilers =
    specs
    |> Compiler.Spec.Set.map ~f:(Compiler.Spec.With_id.id)
    |> Id.Set.of_list
  in
  Tester_config.make
    ~fnames
    ~in_root
    ~out_root
    ~compilers
    ~c_litmus_mode
    ()
;;

let run should_time ~(in_root_raw : string) ~(out_root_raw : string) o cfg =
  let open Or_error.Let_syntax in
  let%bind tester_cfg =
    make_tester_config ~in_root_raw ~out_root_raw o cfg
  in
  let specs = Config.M.compilers cfg in
  let specs_by_machine = group_specs_by_machine specs in

  let start_time = Time.now () in
  let%bind results_alist =
    specs_by_machine
    |> List.map
      ~f:(run_machine o should_time cfg tester_cfg)
    |> Or_error.combine_errors
  in
  let end_time = Time.now () in

  let analysis = Analysis.make
      ~machines:results_alist
      ~time_taken:(Time.diff end_time start_time)
      ()
  in

  let%map table = Analysis.to_table analysis in
  Format.open_vbox 0;
  Tabulator.pp Format.std_formatter table;
  Format.close_box ();
  Format.print_newline ();
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
      and sanitiser_passes = Args.sanitiser_passes
      and compiler_predicate = Args.compiler_predicate
      and machine_predicate = Args.machine_predicate
      and in_root_raw =
        anon ("RESULTS_PATH" %: string)
      in
      fun () ->
        Common.lift_command standard_args
          ?compiler_predicate
          ?machine_predicate
          ?sanitiser_passes
          ~with_compiler_tests:true
          ~f:(fun _args -> run time ~in_root_raw ~out_root_raw)
    ]
;;
