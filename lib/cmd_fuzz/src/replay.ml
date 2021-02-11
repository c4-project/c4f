(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

let run ?(state_output : Plumbing.Output.t option)
    (args : _ Common_cmd.Args.With_files.t) (o : C4f_common.Output.t)
    ~(trace_input : Plumbing.Input.t) : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind trace = C4f_fuzz.Trace.load trace_input in
    let aux = C4f_fuzz_run.Filter.Aux.Replay.make ~o trace in
    (* There's no point exposing the trace here; it'll be the same as the
       input one. *)
    let%bind {state; _} =
      Common_cmd.Args.With_files.run_filter
        (C4f_fuzz_run.Filter.run_replay ~aux)
        args
    in
    Plumbing.Output.with_opt state_output ~f:(fun dest ->
        C4f_fuzz.State.Dump.store state ~dest ))

let readme () : string =
  C4f_utils.My_string.format_for_readme
    {|
This command takes a C litmus test and fuzzer trace as input, applies
the mutations listed in the trace to the test, and outputs the resulting modified test.
|}

let command : Command.t =
  Command.basic ~summary:"replay a fuzzing trace on a C litmus test" ~readme
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get Standard.get)
      and trace_input =
        flag "trace"
          (required Common_cmd.Args.input_type)
          ~doc:"FILE read a trace of completed fuzz actions to this filename"
      and state_output =
        flag "state-output"
          (optional Common_cmd.Args.output_type)
          ~doc:
            "FILE if given, dump a summary of the final fuzzer state to \
             this filename"
      in
      fun () ->
        Common_cmd.Args.Standard.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ~trace_input ?state_output))
