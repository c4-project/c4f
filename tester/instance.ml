(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

open Core_kernel
open Utils
open Lib
open C
include Instance_intf

module Make_compiler (B : Basic_compiler) : Compiler = struct
  include B
  module C_id = Config.Compiler.Spec.With_id
  module P_file = Pathset.File

  let emits = C_id.emits cspec
  let id = C_id.id cspec

  let bracket f ~stage ~file =
    Output.log_stage o ~stage ~file id;
    let f' () =
      Or_error.tag_arg (f ()) "While executing tester stage" stage String.sexp_of_t
    in
    T.bracket_join f'
  ;;

  (** [herd_run_result] summarises the result of running Herd. *)
  type herd_run_result =
    [ `Success of Herd_output.t
    | `Disabled
    | `Errored
    ]

  let run_herd herd ~(input_path : Fpath.t) ~(output_path : Fpath.t) =
    let result =
      Or_error.tag
        ~tag:"While running herd"
        (Herd.run_and_load_results herd ~input_path ~output_path)
    in
    match result with
    | Result.Ok herd -> `Success herd
    | Result.Error err ->
      Fmt.pf o.wf "@[<v 4>Herd analysis error:@,%a@]@." Error.pp err;
      `Errored
  ;;

  let make_herd_arch = function
    | `C -> Herd.C
    | `Assembly -> Assembly emits
  ;;

  let actually_try_run_herd config arch ~input_path ~output_path =
    match Herd.create ~config ~arch with
    | Ok herd -> run_herd herd ~input_path ~output_path
    | Error e ->
      Fmt.pf o.wf "@[<v 4>Herd configuration error:@,%a@]@." Error.pp e;
      `Errored
  ;;

  (** [try_run_herd arch ~input_path ~output_path]
      sees if [cspec] asked for a Herd run and, if so (and [herd_cfg]
      is [Some]), runs Herd on [infile], outputting to [outfile] and
      using models for [c_or_asm]. *)
  let try_run_herd c_or_asm ~input_path ~output_path =
    match C_id.herd cspec, herd_cfg with
    | false, _ | true, None -> `Disabled
    | true, Some config ->
      actually_try_run_herd config (make_herd_arch c_or_asm) ~input_path ~output_path
  ;;

  (** [lower_thread_local_symbol] converts thread-local symbols of the
      form `0:r0` into the memalloy witness equivalent, `t0r0`. *)
  let lower_thread_local_symbol sym = Litmus.Id.(global (to_memalloy_id sym))

  let analyse
      (c_herd : herd_run_result)
      (a_herd : herd_run_result)
      (locmap : (Litmus.Id.t, Litmus.Id.t) List.Assoc.t)
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
        Herd_output.outcome_of
          ~initial
          ~final
          ~locmap:(fun final_sym ->
            Or_error.return
              (List.Assoc.find ~equal:[%equal: Litmus.Id.t] locmap_r final_sym))
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
      |> List.map ~f:(fun s -> s, lower_thread_local_symbol s)
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
    List.filter_map a ~f:(fun (k, v) ->
        Option.Monad_infix.(List.Assoc.find ~equal b v >>| Tuple2.create k))
  ;;

  module Timing_set = struct
    type elt = Time.Span.t

    type t =
      { in_delitmusify : elt option
      ; in_c_herd : elt option
      ; in_cc : elt option
      ; in_litmusify : elt option
      ; in_a_herd : elt option
      }

    let make ?delitmusify ?c_herd ~cc ~litmusify ~a_herd =
      Option.
        { in_delitmusify = delitmusify >>= T.time_taken
        ; in_c_herd = c_herd >>= T.time_taken
        ; in_cc = cc |> T.time_taken
        ; in_litmusify = litmusify |> T.time_taken
        ; in_a_herd = a_herd |> T.time_taken
        }
    ;;
  end

  let compile_from_pathset_file (fs : Pathset.File.t) =
    bracket ~stage:"CC" ~file:(P_file.name fs) (fun () ->
        C.compile ~infile:(P_file.c_path fs) ~outfile:(P_file.asm_path fs))
  ;;

  let litmusify_single (fs : Pathset.File.t) (cvars : string list) =
    let open Or_error.Let_syntax in
    bracket ~stage:"LITMUS" ~file:(P_file.name fs) (fun () ->
        let%map output =
          R.Litmusify.run_from_fpaths
            (Asm_job.make ~passes:sanitiser_passes ~symbols:cvars ())
            ~infile:(Some (P_file.asm_path fs))
            ~outfile:(Some (P_file.lita_path fs))
        in
        Asm_job.Output.warn o.wf output;
        Asm_job.Output.symbol_map output)
  ;;

  let c_herd_on_pathset_file fs =
    bracket ~stage:"HERD(C)" ~file:(P_file.name fs) (fun () ->
        Or_error.return
          (try_run_herd
             `C
             ~input_path:(P_file.litc_path fs)
             ~output_path:(P_file.herdc_path fs)))
  ;;

  let a_herd_on_pathset_file fs =
    bracket ~stage:"HERD(asm)" ~file:(P_file.name fs) (fun () ->
        Or_error.return
          (try_run_herd
             `Assembly
             ~input_path:(P_file.lita_path fs)
             ~output_path:(P_file.herda_path fs)))
  ;;

  let cvar_from_loc_map_entry (id : Litmus.Id.t) : string Or_error.t =
    let open Or_error.Let_syntax in
    let%map global_id =
      Result.of_option
        (Litmus.Id.as_global id)
        ~error:(Error.of_string "Internal error: got a non-local C location")
    in
    C_identifier.to_string global_id
  ;;

  let cvars_from_loc_map (map : (Litmus.Id.t, Litmus.Id.t) List.Assoc.t)
      : string list Or_error.t =
    map
    |> List.map ~f:(fun (_, id) -> cvar_from_loc_map_entry id)
    |> Or_error.combine_errors
  ;;

  let lift_str_map (str_map : (string, string) List.Assoc.t)
      : (Litmus.Id.t, Litmus.Id.t) List.Assoc.t Or_error.t =
    str_map
    |> List.map ~f:(fun (x, y) ->
           Or_error.both (Litmus.Id.global_of_string x) (Litmus.Id.global_of_string y))
    |> Or_error.combine_errors
  ;;

  (** [map_location_renamings locs sym_redirects] works out the
      mapping from locations in the original C program to
      symbols in the Litmus output by using the redirects table
      from the litmusifier. *)
  let map_location_renamings
      (litc_to_c : (Litmus.Id.t, Litmus.Id.t) List.Assoc.t)
      (c_to_lita : (Litmus.Id.t, Litmus.Id.t) List.Assoc.t)
      : (Litmus.Id.t, Litmus.Id.t) List.Assoc.t =
    compose_alists litc_to_c c_to_lita Litmus.Id.equal
  ;;

  let delitmusify_needed : bool Lazy.t =
    lazy (Input_mode.must_delitmusify (Pathset.input_mode ps))
  ;;

  let delitmusify (fs : Pathset.File.t) : unit Or_error.t =
    let open Or_error.Let_syntax in
    let%map _ =
      Filters.Litmus.run_from_fpaths
        Filters.Delitmus
        ~infile:(Some (P_file.litc_path fs))
        ~outfile:(Some (P_file.c_path fs))
      (* TODO(@MattWindsor91): use the output from this instead of
           needing to run Herd. *)
    in
    ()
  ;;

  let delitmusify_if_needed (fs : Pathset.File.t) : unit T.t Or_error.t =
    bracket
      (fun () ->
        Travesty.T_or_error.when_m (Lazy.force delitmusify_needed) ~f:(fun () ->
            delitmusify fs))
      ~stage:"delitmusifying"
      ~file:(P_file.name fs)
  ;;

  let run_single_from_pathset_file (fs : Pathset.File.t) =
    let open Or_error.Let_syntax in
    (* NB: many of these stages depend on earlier stages' filesystem
       side-effects.  These dependencies aren't evident in the binding
       chain, so be careful when re-ordering. *)
    let%bind c_herd = c_herd_on_pathset_file fs in
    let c_herd_outcome = T.value c_herd in
    let litc_to_c_map = locations_of_herd_result c_herd_outcome in
    let%bind cvars = cvars_from_loc_map litc_to_c_map in
    let%bind delitmusify = delitmusify_if_needed fs in
    let%bind cc = compile_from_pathset_file fs in
    let%bind litmusify = litmusify_single fs cvars in
    let c_to_lita_str_map = T.value litmusify in
    let%bind c_to_lita_map = lift_str_map c_to_lita_str_map in
    let litc_to_lita_map = map_location_renamings litc_to_c_map c_to_lita_map in
    let%bind a_herd = a_herd_on_pathset_file fs in
    let a_herd_outcome = T.value a_herd in
    let%map analysis = analyse c_herd_outcome a_herd_outcome litc_to_lita_map in
    let timings = Timing_set.make ~c_herd ~delitmusify ~cc ~litmusify ~a_herd in
    analysis, timings
  ;;

  let run_single (fs : Pathset.File.t) =
    let open Or_error.Let_syntax in
    let%map result = T.bracket_join (fun () -> run_single_from_pathset_file fs) in
    let herd, timings = T.value result in
    ( Pathset.File.name fs
    , Analysis.File.make
        ~herd
        ?time_taken:(T.time_taken result)
        ?time_taken_in_cc:timings.in_cc
        () )
  ;;

  let run () =
    let open Or_error.Let_syntax in
    let%map files_and_time =
      T.bracket_join (fun () ->
          ps |> Pathset.to_files |> List.map ~f:run_single |> Or_error.combine_errors)
    in
    Analysis.Compiler.make
      ~files:(T.value files_and_time)
      ?time_taken:(T.time_taken files_and_time)
      ()
  ;;
end

(** [Make_machine] makes a single-machine test runner from a
   [Basic_machine]. *)
module Make_machine (B : Basic_machine) : Machine = struct
  include B

  let make_pathset (cfg : Run_config.t)
                   (spec : Config.Compiler.Spec.With_id.t)
      : Pathset.t Or_error.t =
    let open Or_error.Let_syntax in
    let id = Config.Compiler.Spec.With_id.id spec in
    let output_root = Run_config.output_root cfg in
    let input_mode = Run_config.input_mode cfg in
    let%map ps = Pathset.make_and_mkdirs id ~output_root ~input_mode in
    Fmt.pf o.vf "%a@." Pathset.pp ps;
    ps
  ;;

  let make_compiler (cfg : Run_config.t)
                    (spec : Config.Compiler.Spec.With_id.t)
      : (module Compiler) Or_error.t =
    let open Or_error.Let_syntax in
    let%bind (module C) = B.Resolve_compiler.from_spec spec in
    let%bind (module R) = B.asm_runner_from_spec spec in
    let%map ps = make_pathset cfg spec in
    (module Make_compiler (struct
       include (B : Basic)
       module C = C
       module R = R

       let ps = ps
       let cspec = spec
    end)
    : Compiler)
  ;;

  let run_compiler (cfg : Run_config.t) (spec : Config.Compiler.Spec.With_id.t) =
    let open Or_error.Let_syntax in
    let id = Config.Compiler.Spec.With_id.id spec in
    let%bind (module TC) = make_compiler cfg spec in
    let%map result = TC.run () in
    id, result
  ;;

  let run_compilers (cfg : Run_config.t)
      : (Config.Id.t, Analysis.Compiler.t) List.Assoc.t Or_error.t =
    compilers
    |> Config.Compiler.Spec.Set.map ~f:(run_compiler cfg)
    |> Or_error.combine_errors
  ;;

  let make_analysis (raw : (Config.Id.t, Analysis.Compiler.t) List.Assoc.t T.t)
      : Analysis.Machine.t =
    Analysis.Machine.make ~compilers:(T.value raw) ?time_taken:(T.time_taken raw) ()
  ;;

  let run (cfg : Run_config.t) : Analysis.Machine.t Or_error.t =
    let open Or_error.Let_syntax in
    let%map compilers_and_time = T.bracket_join (fun () -> run_compilers cfg) in
    make_analysis compilers_and_time
  ;;
end

let group_specs_by_machine specs =
  specs
  |> Config.Compiler.Spec.Set.group ~f:(fun spec ->
         Config.Machine.Spec.With_id.id (Config.Compiler.Spec.With_id.machine spec))
  |> Config.Id.Map.to_alist
;;

module Make (B : Basic) : S = struct
  include B

  let make_analysis (raw : (Config.Id.t, Analysis.Machine.t) List.Assoc.t T.t)
      : Analysis.t =
    Analysis.make ~machines:(T.value raw) ?time_taken:(T.time_taken raw) ()
  ;;

  let make_machine_module (mach_compilers : Config.Compiler.Spec.Set.t) =
    (module Make_machine (struct
       include B

       (* Reduce the set of compilers to those specifically used in
            this machine. *)
       let compilers = mach_compilers
    end)
    : Machine)
  ;;

  let run_machine (tester_cfg : Run_config.t)
                  (mach_id, mach_compilers) =
    let open Or_error.Let_syntax in
    let (module TM) = make_machine_module mach_compilers in
    let%map analysis = TM.run tester_cfg in
    mach_id, analysis
  ;;

  let run_machines
      (cfg : Run_config.t)
      (specs_by_machine : (Config.Machine.Id.t, Config.Compiler.Spec.Set.t) List.Assoc.t)
      : (Config.Machine.Id.t, Analysis.Machine.t) List.Assoc.t Or_error.t =
    specs_by_machine |> List.map ~f:(run_machine cfg) |> Or_error.combine_errors
  ;;

  let run (cfg : Run_config.t) : Analysis.t Or_error.t =
    let open Or_error.Let_syntax in
    let enabled_ids = Run_config.compilers cfg in
    let specs = Config.Compiler.Spec.Set.restrict compilers enabled_ids in
    let specs_by_machine = group_specs_by_machine specs in
    let%map machines_and_time =
      T.bracket_join (fun () -> run_machines cfg specs_by_machine)
    in
    make_analysis machines_and_time
  ;;
end
