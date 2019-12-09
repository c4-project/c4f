(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core (* for Filename.arg_type *)

let write_trace (trace : Act_fuzz.Trace.t) (oc : Stdio.Out_channel.t) :
    unit Or_error.t =
  Sexp.output_hum oc [%sexp (trace : Act_fuzz.Trace.t)] ;
  Ok ()

let run ?(seed : int option) ?(trace_output : Plumbing.Output.t option)
    (args : _ Common_cmd.Args.With_files.t) (o : Act_common.Output.t)
    (global_config : Act_config.Global.t) : unit Or_error.t =
  let config = Act_config.Global.fuzz global_config in
  let aux_in = Act_fuzz.Filter.Aux.make ~o ~config seed in
  Or_error.Let_syntax.(
    let%bind aux_out =
      Common_cmd.Args.With_files.run_filter
        (module Act_fuzz.Filter.Random)
        args ~aux_in
    in
    Plumbing.Output.with_output_opt trace_output ~f:(write_trace aux_out))

let readme () : string =
  Act_utils.My_string.format_for_readme
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
      in
      fun () ->
        Common_cmd.Common.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ?seed ?trace_output))
