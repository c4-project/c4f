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

include Tester_intf

module Make_compiler (B : Basic_compiler) : Compiler = struct
  include B
  module C_id   = Compiler.Spec.With_id
  module P_file = Pathset.File

  let emits = C_id.emits cspec
  let id    = C_id.id    cspec

  let bracket f ~stage ~file =
    Output.log_stage o ~stage ~file id;
    let f' () = Or_error.tag_arg
        (f ()) "While executing tester stage"
        stage String.sexp_of_t
    in
    T.bracket_join f'
  ;;

  (** [herd_run_result] summarises the result of running Herd. *)
  type herd_run_result =
    [ `Success of Herd_output.t
    | `Disabled
    | `Errored
    ]
  ;;

  let run_herd herd ~(input_path : Fpath.t) ~(output_path : Fpath.t) =
    let result =
      Or_error.tag ~tag:"While running herd"
        (Herd.run_and_load_results herd ~input_path ~output_path)
    in
    match result with
    | Result.Ok herd -> `Success herd
    | Result.Error err ->
      Fmt.pf o.wf "@[<v 4>Herd analysis error:@,%a@]@."
        Error.pp err;
      `Errored
  ;;

  let make_herd_arch = function
    | `C -> Herd.C
    | `Assembly -> Assembly emits
  ;;

  let should_run_herd = C_id.herd cspec

  (** [try_run_herd arch ~input_path ~output_path]
      sees if [cspec] asked for a Herd run and, if so (and [herd_cfg]
      is [Some]), runs Herd on [infile], outputting to [outfile] and
      using models for [c_or_asm]. *)
  let try_run_herd c_or_asm ~input_path ~output_path =
    match should_run_herd, herd_cfg with
    | false, _ | true, None -> `Disabled
    | true, Some config ->
      let arch = make_herd_arch c_or_asm in
      match Herd.create ~config ~arch with
      | Ok herd ->
        run_herd herd ~input_path ~output_path
      | Error e ->
        Fmt.pf o.wf "@[<v 4>Herd configuration error:@,%a@]@."
          Error.pp e;
        `Errored
  ;;

  (** [lower_thread_local_symbol] converts thread-local symbols of the
      form `0:r0` into the memalloy witness equivalent, `t0r0`. *)
  let lower_thread_local_symbol sym =
    Litmus.Ast.Primitives.Id.(
      Global (to_memalloy_id sym)
    )
  ;;

  let analyse
      (c_herd : herd_run_result)
      (a_herd : herd_run_result)
      (locmap : (Litmus.Ast.Primitives.Id.t, Litmus.Ast.Primitives.Id.t) List.Assoc.t)
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
                List.Assoc.find ~equal:Litmus.Ast.Primitives.Id.equal locmap_r final_sym
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

  type timings =
    { in_c_herd    : Time.Span.t option
    ; in_cc        : Time.Span.t option
    ; in_litmusify : Time.Span.t option
    ; in_a_herd    : Time.Span.t option
    }
  ;;

  let compile_from_pathset_file (fs : Pathset.File.t) =
    bracket ~stage:"CC" ~file:(P_file.basename fs)
      (fun () ->
         C.compile
           ~infile:(P_file.c_path fs)
           ~outfile:(P_file.asm_path fs))
  ;;

  let litmusify_single (fs : Pathset.File.t) (cvars : string list) =
    let open Or_error.Let_syntax in
    bracket ~stage:"LITMUS" ~file:(P_file.basename fs)
      (fun () ->
         let%map output =
           R.Litmusify.run_from_fpaths
             (Asm_job.make ~passes:sanitiser_passes ~symbols:cvars ())
             ~infile:(Some (P_file.asm_path fs))
             ~outfile:(Some (P_file.lita_path fs))
         in
         Asm_job.warn output o.wf;
         Asm_job.symbol_map output
      )
  ;;

  let c_herd_on_pathset_file fs =
    bracket ~stage:"HERD(C)" ~file:(P_file.basename fs)
      (fun () -> Or_error.return (
           try_run_herd `C
             ~input_path:(P_file.litc_path fs)
             ~output_path:(P_file.herdc_path fs)))
  ;;

  let a_herd_on_pathset_file fs =
    bracket ~stage:"HERD(asm)" ~file:(P_file.basename fs)
      (fun () -> Or_error.return (
           try_run_herd `Assembly
             ~input_path:(P_file.lita_path fs)
             ~output_path:(P_file.herda_path fs)))
  ;;

  let make_timings c_herd cc litmusify a_herd =
    { in_c_herd    = T.time_taken c_herd
    ; in_cc        = T.time_taken cc
    ; in_litmusify = T.time_taken litmusify
    ; in_a_herd    = T.time_taken a_herd
    }
  ;;

  module L_id = Litmus.Ast.Primitives.Id

  let cvars_from_loc_map
      (map : (L_id.t, L_id.t) List.Assoc.t)
    : string list Or_error.t =
    map
    |> List.map ~f:(
      function
      | (_, L_id.Global c_sym) ->
        Or_error.return (Utils.C_identifier.to_string c_sym)
      | _ ->
        Or_error.error_string
          "Internal error: got a non-local C location"
    )
    |> Or_error.combine_errors
  ;;

  let lift_str_map
    (str_map : (string, string) List.Assoc.t)
    : (L_id.t, L_id.t) List.Assoc.t Or_error.t =
    str_map
    |> List.map ~f:(
      fun (x, y) ->
        Or_error.(
          both
            (x |> Utils.C_identifier.create >>| fun x -> L_id.Global x)
            (y |> Utils.C_identifier.create >>| fun y -> L_id.Global y)
        )
    )
    |> Or_error.combine_errors
  ;;

  (** [map_location_renamings locs sym_redirects] works out the
      mapping from locations in the original C program to
      symbols in the Litmus output by using the redirects table
      from the litmusifier. *)
  let map_location_renamings
      (litc_to_c : (L_id.t, L_id.t) List.Assoc.t)
      (c_to_lita : (L_id.t, L_id.t) List.Assoc.t)
    : (L_id.t, L_id.t) List.Assoc.t =
    compose_alists litc_to_c c_to_lita L_id.equal
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
    let%bind cc = compile_from_pathset_file fs in
    let%bind litmusify = litmusify_single fs cvars in
    let c_to_lita_str_map = T.value litmusify in
    let%bind c_to_lita_map = lift_str_map c_to_lita_str_map in
    let litc_to_lita_map = map_location_renamings litc_to_c_map c_to_lita_map in
    let%bind a_herd = a_herd_on_pathset_file fs in
    let a_herd_outcome = T.value a_herd in
    let%map analysis = analyse c_herd_outcome a_herd_outcome litc_to_lita_map in
    let timings = make_timings c_herd cc litmusify a_herd in
    (analysis, timings)
  ;;

  let run_single (ps : Pathset.t) (c_fname : string) =
    let open Or_error.Let_syntax in
    let fs = Pathset.File.make ps c_fname in
    let%map result =
      T.bracket_join (fun () -> run_single_from_pathset_file fs)
    in
    let (herd, timings) = T.value result in
    ( c_fname
    , Analysis.File.make
        ~herd
        ?time_taken:(T.time_taken result)
        ?time_taken_in_cc:(timings.in_cc)
        ()
    )
  ;;

  let run (c_fnames : string list) =
    let open Or_error.Let_syntax in
    let%map files_and_time =
      T.bracket_join (fun () ->
          c_fnames
          |> List.map ~f:(run_single ps)
          |> Or_error.combine_errors
        )
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

  let run_compiler (cfg : Tester_config.t) cspec =
    let module Cfg = Tester_config in
    let open Or_error.Let_syntax in
    let id = Compiler.Spec.With_id.id cspec in
    let c_fnames = Cfg.fnames cfg in
    let in_root  = Cfg.in_root cfg in
    let out_root = Cfg.out_root cfg in
    let%bind (module C) = B.Resolve_compiler.from_spec cspec in
    let%bind (module R) = B.asm_runner_from_spec cspec in
    let%bind ps = Pathset.make_and_mkdirs id ~in_root ~out_root in
    let module TC = Make_compiler (struct
        include (B : Basic)
        module C = C
        module R = R
        let ps = ps
        let cspec = cspec
      end) in
    Pathset.pp o.vf ps;
    Format.pp_print_newline o.vf ();
    let%map result = TC.run c_fnames in
    (id, result)
  ;;

  let spec_id_in (ids : Id.Set.t) (spec : Compiler.Spec.With_id.t) : bool =
    Id.Set.mem ids (Compiler.Spec.With_id.id spec)

  let filter_specs (ids : Id.Set.t) : Compiler.Spec.Set.t Or_error.t =
    compilers
    |> Compiler.Spec.Set.map
      ~f:(fun spec -> Option.some_if (spec_id_in ids spec) spec)
    |> List.filter_opt
    |> Compiler.Spec.Set.of_list
  ;;

  let run (cfg : Tester_config.t) : Analysis.Machine.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind specs = filter_specs (Tester_config.compilers cfg) in
    let%map compilers_and_time =
      T.bracket_join (fun () ->
          specs
          |> Compiler.Spec.Set.map
            ~f:(run_compiler cfg)
          |> Or_error.combine_errors
        )
    in
    Analysis.Machine.make
      ~compilers:(T.value compilers_and_time)
      ?time_taken:(T.time_taken compilers_and_time)
      ()
  ;;
end
