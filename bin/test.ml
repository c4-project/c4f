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

let find_filenames : Tester.Run_config.C_litmus_mode.t -> Fpath.t -> string list Or_error.t =
  function
  | Memalloy -> find_memalloy_c_filenames
  | Delitmusify -> find_delitmusify_litmus_filenames
;;

let make_tester_config
    ~(in_root_raw : string) ~(out_root_raw : string)
    ~(c_litmus_mode : Tester.Run_config.C_litmus_mode.t)
    o cfg :
  Tester.Run_config.t Or_error.t =
  let open Or_error.Let_syntax in
  let%bind in_root  = Io.fpath_of_string in_root_raw
  and      out_root = Io.fpath_of_string out_root_raw
  in
  let%bind fnames = find_filenames c_litmus_mode in_root in
  let specs = Config.M.compilers cfg in
  report_spec_errors o
    (List.filter_map ~f:snd (Config.M.disabled_compilers cfg));
  let compilers =
    specs
    |> Compiler.Spec.Set.map ~f:(Compiler.Spec.With_id.id)
    |> Id.Set.of_list
  in
  Tester.Run_config.make
    ~fnames
    ~in_root
    ~out_root
    ~compilers
    ~c_litmus_mode
    ()
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

let run should_time c_litmus_mode ~(in_root_raw : string) ~(out_root_raw : string) o cfg =
  let open Or_error.Let_syntax in
  let%bind tester_cfg =
    make_tester_config ~in_root_raw ~out_root_raw ~c_litmus_mode o cfg
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
      and c_litmus_mode =
        Tester.Run_config.C_litmus_mode.(
          choose_one
            [ Args.flag_to_enum_choice
                Memalloy
                "memalloy"
                ~doc:"assume the input directory has the layout of a Memalloy run result"
            ; Args.flag_to_enum_choice
                Delitmusify
                "delitmusify"
                ~doc:"assume the input directory only contains litmus tests"
            ]
            ~if_nothing_chosen:(`Default_to Memalloy)
        )
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
          ~f:(fun _args -> run time c_litmus_mode ~in_root_raw ~out_root_raw)
    ]
;;
