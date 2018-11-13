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

(** [herd_run_result] summarises the result of running Herd. *)
type herd_run_result =
  [ `Success of Herd_output.t
  | `Disabled
  | `Errored
  ]
;;

let run_herd (o : Output.t) herd arch ~input_path ~output_path =
  let result =
    Or_error.tag ~tag:"While running herd"
      (Herd.run_and_load_results herd arch ~input_path ~output_path)
  in
  match result with
  | Result.Ok herd -> `Success herd
  | Result.Error err ->
    Format.fprintf o.wf "@[<v 4>Herd analysis error:@,%a@]@."
      Error.pp err;
    `Errored
;;

(** [try_run_herd o maybe_herd arch ~input_path ~output_path cspec]
   sees if [cspec] asked for a Herd run and, if so (and [maybe_herd]
   is [Some]), runs Herd on [infile], outputting to [outfile] and
   using models for [c_or_asm]. *)
let try_run_herd o maybe_herd c_or_asm ~input_path ~output_path cspec =
  let id = Compiler.Spec.With_id.id cspec in
  let should_run_herd = Compiler.Spec.With_id.herd cspec in
  match should_run_herd, maybe_herd with
  | false, _ | true, None -> `Disabled
  | true, Some herd -> begin
      Output.log_stage o ~stage:"HERD" ~file:input_path id;
      let arch = match c_or_asm with
        | `C -> Herd.C
        | `Assembly -> Assembly (Compiler.Spec.With_id.emits cspec)
      in
      run_herd o herd arch ~input_path ~output_path
    end
;;

(** [lower_thread_local_symbol] converts thread-local symbols of the
   form `0:r0` into the memalloy witness equivalent, `t0r0`. *)
let lower_thread_local_symbol sym =
  Option.value ~default:sym
    ( let open Option.Let_syntax in
      let%bind (thread, rest) = String.lsplit2 ~on:':' sym in
      let%bind tnum = Caml.int_of_string_opt thread in
      let%map tnum = Option.some_if (Int.is_non_negative tnum) tnum in
      sprintf "t%d%s" tnum rest
    )
;;

let analyse
    (c_herd : herd_run_result)
    (a_herd : herd_run_result)
    (locmap : (string, string) List.Assoc.t)
  : Analysis.Herd.t Or_error.t =
  let open Or_error.Let_syntax in
  (* The locmap function we supply below goes backwards, from
     assembly locations to C symbols.  Our location map goes the other
     way, so we need to transpose it. *)
  let locmap_r = List.Assoc.inverse locmap in
  match c_herd, a_herd with
  | `Disabled, _ | _, `Disabled -> return `Disabled
  | `Errored, _ -> return (`Errored `C)
  | _, `Errored -> return (`Errored `Assembly)
  | `Success initial, `Success final ->
    let%map outcome =
      Herd_output.outcome_of ~initial ~final
        ~locmap:(
          fun final_sym ->
            Or_error.return (
              List.Assoc.find ~equal:String.equal locmap_r final_sym
            )
        )
        (* TODO(@MattWindsor91): properly valmap, if needed. *)
        ~valmap:return
      in
      (outcome :> Analysis.Herd.t)
;;

(** [locations_of_herd_result r] takes the C/litmus Herd result [r]
   and extracts an associative array of mappings [(s, s')] where each
   [s] is a location mentioned in [r]'s state list, and each [s'] is
   the equivalent variable name in the memalloy C witness. *)
let locations_of_herd_result = function
  | `Success herd ->
    Herd_output.states herd
    |> List.concat_map ~f:Herd_output.State.bound
    |> List.map ~f:(fun s -> (s, lower_thread_local_symbol s))
  | `Disabled | `Errored -> []
;;

(** [compose_alists a b equal] produces an associative list that
    returns [(x, z)] for each [(x, y)] in [a] such that
    a [(y', z)] exists in [b] and [equal y y']. *)
let compose_alists
  (a : ('a, 'b) List.Assoc.t)
  (b : ('b, 'c) List.Assoc.t)
  (equal : 'b -> 'b -> bool)
  : ('a, 'c) List.Assoc.t =
  List.filter_map a ~f:(
    fun (k, v) ->
      Option.Monad_infix.(
        List.Assoc.find ~equal b v >>| Tuple2.create k
      )
  )
;;

let litmusify_single
    (o : Output.t) (fs : Pathset.File.t) locations cspec =
  (* The location symbols at the C level are each RHS of each
     pair in locs. *)
  let syms = List.map ~f:snd locations in
  let id   = Compiler.Spec.With_id.id cspec in
  let inp  = `File (Pathset.File.asm_path fs) in
  let outp = `File (Pathset.File.lita_path fs) in
  Output.log_stage o ~stage:"LITMUS" ~file:(Pathset.File.basename fs) id;
  Common.litmusify o inp outp syms (`Spec cspec)
;;

(** [map_location_renamings locs sym_redirects] works out the
    mapping from locations in the original C program to
    symbols in the Litmus output by using the redirects table
    from the litmusifier. *)
let map_location_renamings locs sym_redirects =
  compose_alists locs sym_redirects String.equal
;;

let compile_from_pathset_file
  (c  : (module Compiler.S))
  (o  : Output.t)
  (fs : Pathset.File.t)
  (cspec : Compiler.Spec.With_id.t) =
  Common.compile_with_compiler c o (Compiler.Spec.With_id.id cspec)
    ~name:(Pathset.File.basename fs)
    ~infile:(Pathset.File.c_path fs)
    ~outfile:(Pathset.File.asm_path fs)
;;

let run_single
    (c : (module Compiler.S))
    (o : Output.t)
    (ps : Pathset.t)
    herd
    spec
    fname =
  let base = Filename.chop_extension (Filename.basename fname) in
  let open Or_error.Let_syntax in
  Pathset.File.(
    let start_time = Time.now () in

    (* NB: many of these stages depend on earlier stages' filesystem
       side-effects.  These dependencies aren't evident in the binding
       chain, so be careful when re-ordering. *)
    let fs = make ps base in
    let c_herd =
      try_run_herd o herd `C spec
        ~input_path:(litc_path fs) ~output_path:(herdc_path fs)
    in
    let locs = locations_of_herd_result c_herd in
    let%bind time_taken_in_cc = compile_from_pathset_file c o fs spec in
    let%bind sym_redirects = litmusify_single o fs locs spec in
    let loc_map = map_location_renamings locs sym_redirects in
    let a_herd =
      try_run_herd o herd `Assembly spec
        ~input_path:(lita_path fs) ~output_path:(herda_path fs)
    in
    let%map analysis = analyse c_herd a_herd loc_map in

    let end_time = Time.now () in

    ( base
    , Analysis.File.create
        ~herd:analysis
        ~time_taken:(Time.diff end_time start_time)
        ~time_taken_in_cc
    )
  )
;;

let run_compiler
    (o : Output.t) ~in_root ~out_root herdprog c_fnames cspec =
  let open Or_error.Let_syntax in
  let id = Compiler.Spec.With_id.id cspec in
  let%bind c = Language_support.compiler_from_spec cspec in
  let%bind paths = Pathset.make_and_mkdirs id ~in_root ~out_root in
  Pathset.pp o.vf paths;
  Format.pp_print_newline o.vf ();

  let start_time = Time.now () in
  let%map results_alist =
    c_fnames
    |> List.sort ~compare:Core_extended.Extended_string.collate
    |> List.map ~f:(run_single c o paths herdprog cspec)
    |> Or_error.combine_errors
  in
  let end_time = Time.now () in

  ( id
  , Analysis.Compiler.create
      ~files:results_alist
      ~time_taken:(Time.diff end_time start_time)
  )
;;

let run_machine
    o ~in_root ~out_root herdprog c_files (mach_id, specs) =
  let open Or_error.Let_syntax in

  let start_time = Time.now () in
  let%map results_alist =
    specs
    |> Compiler.Spec.Set.map
      ~f:(run_compiler o ~in_root ~out_root herdprog c_files)
    |> Or_error.combine_errors
  in
  let end_time = Time.now () in

  ( mach_id
  , Analysis.Machine.create
      ~compilers:results_alist
      ~time_taken:(Time.diff end_time start_time)
  )
;;

let check_c_files_exist c_path c_files =
    if List.is_empty c_files
    then Or_error.error_s
        [%message "Expected at least one C file." ~path:c_path]
    else Result.ok_unit
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

let make_herd =
  Or_error.(
    Option.value_map
      ~default:(return None)
      ~f:(fun config -> Herd.create ~config >>| Option.some)
  )
;;

let run ~in_root ~out_root o cfg =
  let open Or_error.Let_syntax in

  let specs = Config.M.compilers cfg in
  report_spec_errors o
    (List.filter_map ~f:snd (Config.M.disabled_compilers cfg));

  let%bind herd = make_herd (Config.M.herd cfg) in

  let c_path = Filename.concat in_root "C" in
  let%bind c_files = Io.Dir.get_files c_path ~ext:"c" in
  let%bind () = check_c_files_exist c_path c_files in

  let specs_by_machine = group_specs_by_machine specs in

  let start_time = Time.now () in
  let%bind results_alist =
    specs_by_machine
    |> List.map ~f:(run_machine o ~in_root ~out_root herd c_files)
    |> Or_error.combine_errors
  in
  let end_time = Time.now () in

  let analysis = Analysis.create
                   ~machines:results_alist
                   ~time_taken:(Time.diff end_time start_time)
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
       let standard_args = Standard_args.get
       and out_root =
         flag_optional_with_default_doc "output"
           ~default:Filename.current_dir_name
           string [%sexp_of: string]
           ~doc: "PATH the path under which output directories will be created"
       and compiler_predicate = Standard_args.Other.compiler_predicate
       and machine_predicate = Standard_args.Other.machine_predicate
       and in_root =
         anon ("RESULTS_PATH" %: string)
       in
       fun () ->
         Common.lift_command standard_args
           ?compiler_predicate
           ?machine_predicate
           ~with_compiler_tests:true
           ~f:(run ~in_root ~out_root)
    ]
;;
