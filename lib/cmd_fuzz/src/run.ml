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
  Result.ok_unit

let run ?(seed : int option) ?(trace_output : string option)
    (args : _ Toplevel.Args.With_files.t) (o : Act_common.Output.t)
    (act_config : Act_config.Act.t) : unit Or_error.t =
  let config =
    act_config |> Act_config.Act.fuzz
    |> Option.value ~default:(Act_config.Fuzz.make ())
  in
  Toplevel.Args.With_files.run_filter_with_aux_out
    (module Act_fuzz.Filter)
    args ~aux_in:{seed; o; config} ~aux_out_f:write_trace
    ?aux_out_filename:trace_output

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
This command takes, as input, a C litmus test.  It then performs various
mutations to the litmus test, and outputs the resulting modified test.
|}

let command : Command.t =
  Command.basic ~summary:"performs fuzzing mutations on a C litmus test"
    ~readme
    Command.Let_syntax.(
      let%map_open standard_args =
        ignore anon ;
        Toplevel.Args.(With_files.get Standard.get)
      and seed =
        flag "seed" (optional int)
          ~doc:"INT use this integer as the seed to the fuzzer RNG"
      and trace_output =
        flag "trace-output"
          (optional Filename.arg_type)
          ~doc:
            "FILE if given, write a trace of completed fuzz actions to \
             this filename"
      in
      fun () ->
        Toplevel.Common.lift_command
          (Toplevel.Args.With_files.rest standard_args)
          ~with_compiler_tests:false
          ~f:(run standard_args ?seed ?trace_output))
