(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

let write_aux ?(trace_output : Plumbing.Output.t option)
    ?(state_output : Plumbing.Output.t option)
    ({state; trace} : C4f_fuzz_run.Filter.Aux.Output.t) : unit Or_error.t =
  Or_error.all_unit
    [ Plumbing.Output.with_opt state_output ~f:(fun dest ->
          C4f_fuzz.State.Dump.store state ~dest )
    ; Plumbing.Output.with_opt trace_output ~f:(fun dest ->
          C4f_fuzz.Trace.store trace ~dest ) ]

let run ?(seed : int option) ?(state_output : Plumbing.Output.t option)
    ?(trace_output : Plumbing.Output.t option)
    (args : _ Common_cmd.Args.With_files.t) (o : C4f_common.Output.t)
    (global_config : C4f_config.Global.t) : unit Or_error.t =
  let config = C4f_config.Global.fuzz global_config in
  let aux = C4f_fuzz_run.Filter.Aux.Randomised.make ~o ?seed config in
  Or_error.(
    Common_cmd.Args.With_files.run_filter
      (C4f_fuzz_run.Filter.run_randomised ~aux)
      args
    >>= write_aux ?state_output ?trace_output)

let readme () : string =
  C4f_utils.My_string.format_for_readme
    {|
This command takes, as input, a C litmus test.  It then performs various
mutations to the litmus test, and outputs the resulting modified test.
|}

let command : Command.t =
  Command.basic ~summary:"perform fuzzing mutations on a C litmus test"
    ~readme
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get Standard.get)
      and seed =
        flag "seed" (optional int)
          ~doc:"INT use this integer as the seed to the fuzzer RNG"
      and trace_output =
        flag "trace-output"
          (optional Common_cmd.Args.output_type)
          ~doc:
            "FILE if given, write a trace of completed fuzz actions to this \
             filename"
      and state_output =
        flag "state-output"
          (optional Common_cmd.Args.output_type)
          ~doc:
            "FILE if given, dump a summary of the final fuzzer state to \
             this filename"
      in
      fun () ->
        Common_cmd.Args.Standard.lift_command_with_config
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ?seed ?trace_output ?state_output))
