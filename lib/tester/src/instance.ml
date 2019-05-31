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
module Tx = Travesty_core_kernel_exts
open Instance_intf
module Machine_assoc =
  Travesty.Bi_mappable.Fix2_left (Tx.Alist) (Act_common.Id)

(** Bundle of everything used in a tester job. *)
module Job = struct
  type t =
    { config: Run_config.t
    ; specs: Act_compiler.Machine_spec.Set.t
    ; c_simulations: Act_sim.Bulk.File_map.t
    ; make_machine:
        Act_compiler.Machine_spec.With_id.t -> (module Machine.S) Or_error.t
    }
  [@@deriving fields, make]

  let run_machine (job : t)
      (mach_spec : Act_compiler.Machine_spec.With_id.t) =
    Or_error.Let_syntax.(
      let%bind (module TM) = make_machine job mach_spec in
      let%map analysis = TM.run (config job) (c_simulations job) in
      (Act_compiler.Machine_spec.With_id.id mach_spec, analysis))

  let run (job : t) : Analysis.Machine.t Machine_assoc.t Or_error.t =
    job |> specs
    |> Act_compiler.Machine_spec.Set.map ~f:(run_machine job)
    |> Or_error.combine_errors
end

module Make (B : Basic) : S = struct
  include B
  include Common.Extend (B)

  let make_analysis (raw : Analysis.Machine.t Machine_assoc.t T.t) :
      Analysis.t =
    Analysis.make ~machines:(T.value raw) ?time_taken:(T.time_taken raw) ()

  let make_machine (spec : Act_compiler.Machine_spec.With_id.t) :
      (module Machine.S) Or_error.t =
    Or_error.Let_syntax.(
      let id = Act_compiler.Machine_spec.With_id.id spec in
      let%map asm_simulators = B.Asm_simulator_resolver.make_table id in
      ( module Machine.Make (struct
        include B

        let spec = spec

        let asm_simulators = asm_simulators
      end)
      : Machine.S ))

  module H = C_sim.Make (B.C_simulator)
  module S = Act_sim.Bulk.Make (B.C_simulator)

  let make_output_path (ps : Pathset.Run.t) : (Fpath.t -> Fpath.t) Staged.t
      =
    Staged.stage (fun input_path ->
        let name = Fpath.(basename (rem_ext input_path)) in
        Pathset.Run.c_sim_file ps name )

  let run_c_simulations (config : Run_config.t) :
      Act_sim.Bulk.File_map.t Or_error.t =
    let input_paths = Run_config.c_litmus_files config in
    let ps = Run_config.pathset config in
    let output_path_f = Staged.unstage (make_output_path ps) in
    let job = {S.Job.arch= C; input_paths; output_path_f} in
    S.run job

  let run_and_time_c_simulations (config : Run_config.t) :
      Act_sim.Bulk.File_map.t B.T.t Or_error.t =
    bracket ~id:B.C_simulator.name ~machine:B.C_simulator.machine_id
      ~stage:"sim" ~sub_stage:"C" (fun () -> run_c_simulations config)

  let make_job (config : Run_config.t) : Job.t Or_error.t =
    Or_error.Let_syntax.(
      (* TODO(@MattWindsor91): do something with the times. *)
      let%map c_simulations_t = run_and_time_c_simulations config in
      let c_simulations = T.value c_simulations_t in
      Job.make ~config ~c_simulations ~specs:B.machines ~make_machine)

  let run_job_timed (job : Job.t) :
      Analysis.Machine.t Machine_assoc.t T.t Or_error.t =
    T.bracket_join (fun () -> Job.run job)

  let run (config : Run_config.t) : Analysis.t Or_error.t =
    Or_error.(config |> make_job >>= run_job_timed >>| make_analysis)
end
