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

open Core_kernel
open Travesty_core_kernel_exts
open Utils
open Lib
open Act_common
open C
include Compiler_intf

module Make (B : Basic) : S = struct
  include B
  include Common.Extend (B)
  module C_id = Config.Compiler.Spec.With_id
  module P_file = Pathset.File

  let emits = C_id.emits cspec

  let id = C_id.id cspec

  (** [lower_thread_local_symbol] converts thread-local symbols of the form
      `0:r0` into the memalloy witness equivalent, `t0r0`. *)
  let lower_thread_local_symbol sym =
    Litmus.Id.(global (to_memalloy_id sym))

  let analyse (c_herd : Sim.Output.t) (a_herd : Sim.Output.t)
      (locmap : (Litmus.Id.t, Litmus.Id.t) List.Assoc.t) :
      Analysis.Herd.t Or_error.t =
    let open Or_error.Let_syntax in
    (* The locmap function we supply below goes backwards, from assembly
       locations to C symbols. Our location map goes the other way, so we
       need to transpose it. *)
    let locmap_r = List.Assoc.inverse locmap in
    match (c_herd, a_herd) with
    | Skipped _, _ | _, Skipped _ ->
        return Analysis.Herd.Disabled
    | Errored _, _ ->
        return (Analysis.Herd.Errored `C)
    | _, Errored _ ->
        return (Analysis.Herd.Errored `Assembly)
    | Success oracle, Success subject ->
        let%map outcome =
          Sim.Diff.run ~oracle ~subject
            ~location_map:(fun final_sym ->
              Or_error.return
                (List.Assoc.find ~equal:[%equal: Litmus.Id.t] locmap_r
                   final_sym) )
              (* TODO(@MattWindsor91): properly valmap, if needed. *)
            ~value_map:return
        in
        Analysis.Herd.Run outcome

  (** [locations_of_herd_result r] takes the C/litmus Herd result [r] and
      extracts an associative array of mappings [(s, s')] where each [s] is
      a location mentioned in [r]'s state list, and each [s'] is the
      equivalent variable name in the memalloy C witness. *)
  let locations_of_herd_result : Sim.Output.t -> _ list = function
    | Success herd ->
        Sim.Output.Observation.states herd
        |> List.concat_map ~f:Sim.State.bound
        |> List.map ~f:(fun s -> (s, lower_thread_local_symbol s))
    | Skipped _ | Errored _ ->
        []

  let compile_from_pathset_file (fs : Pathset.File.t) =
    bracket ~id ~stage:"CC" ~file:(P_file.name fs) (fun () ->
        C.compile ~infile:(P_file.c_file fs) ~outfile:(P_file.asm_file fs)
    )

  let litmusify_single (fs : Pathset.File.t) (cvars : string list) =
    let open Or_error.Let_syntax in
    bracket ~id ~stage:"LITMUS" ~file:(P_file.name fs) (fun () ->
        let%map output =
          R.Litmusify.run_from_fpaths
            (Asm_job.make ~passes:sanitiser_passes ~symbols:cvars ())
            ~infile:(Some (P_file.asm_file fs))
            ~outfile:(Some (P_file.asm_litmus_file fs))
        in
        Output.pw o "@[%a@]@." Asm_job.Output.warn output ;
        Asm_job.Output.symbol_map output )

  let a_herd_on_pathset_file (fs : Pathset.File.t) :
      Sim.Output.t T.t Or_error.t =
    bracket ~id ~stage:"SIM(asm)" ~file:(P_file.name fs) (fun () ->
        B.Asm_simulator.run (Sim.Arch.Assembly emits)
          ~input_path:(P_file.asm_litmus_file fs)
          ~output_path:(P_file.asm_sim_file fs) )

  let cvar_from_loc_map_entry (id : Litmus.Id.t) : string Or_error.t =
    let open Or_error.Let_syntax in
    let%map global_id =
      Result.of_option (Litmus.Id.as_global id)
        ~error:
          (Error.of_string "Internal error: got a non-local C location")
    in
    C_identifier.to_string global_id

  let cvars_from_loc_map (map : (Litmus.Id.t, Litmus.Id.t) List.Assoc.t) :
      string list Or_error.t =
    map
    |> List.map ~f:(fun (_, id) -> cvar_from_loc_map_entry id)
    |> Or_error.combine_errors

  let lift_str_map (str_map : (string, string) List.Assoc.t) :
      (Litmus.Id.t, Litmus.Id.t) List.Assoc.t Or_error.t =
    str_map
    |> List.map ~f:(fun (x, y) ->
           Or_error.both
             (Litmus.Id.global_of_string x)
             (Litmus.Id.global_of_string y) )
    |> Or_error.combine_errors

  (** [map_location_renamings locs sym_redirects] works out the mapping from
      locations in the original C program to symbols in the Litmus output by
      using the redirects table from the litmusifier. *)
  let map_location_renamings
      (litc_to_c : (Litmus.Id.t, Litmus.Id.t) List.Assoc.t)
      (c_to_lita : (Litmus.Id.t, Litmus.Id.t) List.Assoc.t) :
      (Litmus.Id.t, Litmus.Id.t) List.Assoc.t =
    Alist.compose litc_to_c c_to_lita ~equal:Litmus.Id.equal

  let delitmusify_needed : bool Lazy.t =
    lazy (Input_mode.must_delitmusify (Pathset.Compiler.input_mode ps))

  let delitmusify (fs : Pathset.File.t) : unit Or_error.t =
    let open Or_error.Let_syntax in
    let%map _ =
      Filters.Litmus.run_from_fpaths Filters.Delitmus
        ~infile:(Some (P_file.c_litmus_file fs))
        ~outfile:(Some (P_file.c_file fs))
      (* TODO(@MattWindsor91): use the output from this instead of needing
         to run Herd. *)
    in
    ()

  let delitmusify_if_needed (fs : Pathset.File.t) : unit T.t Or_error.t =
    bracket
      (fun () ->
        Or_error.when_m (Lazy.force delitmusify_needed) ~f:(fun () ->
            delitmusify fs ) )
      ~stage:"delitmusifying" ~file:(P_file.name fs) ~id

  let run_single_from_pathset_file (c_sims : Sim.Bulk.File_map.t)
      (fs : Pathset.File.t) : (Analysis.Herd.t * TS.t) Or_error.t =
    let open Or_error.Let_syntax in
    (* NB: many of these stages depend on earlier stages' filesystem
       side-effects. These dependencies aren't evident in the binding chain,
       so be careful when re-ordering. *)
    let litmus_path = Pathset.File.c_litmus_file fs in
    let c_herd_outcome = Sim.Bulk.File_map.get c_sims ~litmus_path in
    let litc_to_c_map = locations_of_herd_result c_herd_outcome in
    let%bind cvars = cvars_from_loc_map litc_to_c_map in
    let%bind delitmusifier = delitmusify_if_needed fs in
    let%bind compiler = compile_from_pathset_file fs in
    let%bind litmusifier = litmusify_single fs cvars in
    let c_to_lita_str_map = T.value litmusifier in
    let%bind c_to_lita_map = lift_str_map c_to_lita_str_map in
    let litc_to_lita_map =
      map_location_renamings litc_to_c_map c_to_lita_map
    in
    let%bind asm_simulator = a_herd_on_pathset_file fs in
    let a_herd_outcome = T.value asm_simulator in
    let%map analysis =
      analyse c_herd_outcome a_herd_outcome litc_to_lita_map
    in
    (* TODO(@MattWindsor91): fix c simulator timing *)
    let timings =
      TS.make ~delitmusifier ~compiler ~litmusifier ~asm_simulator ()
    in
    (analysis, timings)

  let run_single (c_sims : Sim.Bulk.File_map.t) (fs : Pathset.File.t) =
    let open Or_error.Let_syntax in
    let%map result =
      T.bracket_join (fun () -> run_single_from_pathset_file c_sims fs)
    in
    let herd, timings = T.value result in
    ( Pathset.File.name fs
    , Analysis.File.make ~herd ?time_taken:(T.time_taken result)
        ?time_taken_in_cc:(TS.in_compiler timings) () )

  let run (c_sims : Sim.Bulk.File_map.t) =
    let open Or_error.Let_syntax in
    let%map files_and_time =
      T.bracket_join (fun () ->
          ps |> Pathset.File.make_all
          |> List.map ~f:(run_single c_sims)
          |> Or_error.combine_errors )
    in
    Analysis.Compiler.make ~files:(T.value files_and_time)
      ?time_taken:(T.time_taken files_and_time)
      ()
end
