(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core (* for Filename.arg_type *)

let run ?(trace_input : string option)
    (args : _ Common_cmd.Args.With_files.t) (o : Act_common.Output.t)
    (global_config : Act_config.Global.t) : unit Or_error.t =
  let config = Act_config.Global.fuzz global_config in
  Or_error.Let_syntax.(
    let%bind trace_in = Plumbing.Input.of_string_opt trace_input in
    let%bind trace = Act_fuzz.Trace.load trace_in in
    let aux_in = Act_fuzz.Filter.Aux.make ~o ~config trace in
    Common_cmd.Args.With_files.run_filter
      (module Act_fuzz.Filter.Replay)
      args ~aux_in)

let readme () : string =
  Act_utils.My_string.format_for_readme
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
          (required Filename.arg_type)
          ~doc:"FILE read a trace of completed fuzz actions to this filename"
      in
      fun () ->
        Common_cmd.Common.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ~trace_input))
