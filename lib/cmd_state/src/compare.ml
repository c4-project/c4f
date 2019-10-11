(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core
open Act_common

let output (result : Act_state.Compare.Result.t) ~(human_readable : bool) :
    unit =
  let pp =
    if human_readable then Act_state.Compare.Result.pp
    else Act_state.Compare.Result.pp_json
  in
  pp Fmt.stdout result

let run (_o : Output.t) (_cfg : Act_config.Global.t) ~(oracle_raw : string)
    ~(subject_raw : string) ~(human_readable : bool) : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind oracle_in = Plumbing.Input.of_string oracle_raw in
    let%bind oracle = Act_state.Observation.load oracle_in in
    let%bind subject_in = Plumbing.Input.of_string subject_raw in
    let%bind subject = Act_state.Observation.load subject_in in
    let%map result = Act_state.Compare.run ~oracle ~subject in
    output result ~human_readable)

let readme () =
  Act_utils.My_string.format_for_readme
    {|
    This command takes two summaries of backend runs: an
    'oracle' (usually a program _before_ compilation), and a
    'subject' (usually the same program _after_ compilation).  It
    then parses the summaries and compares the sets of final states on both
    ends.

    Both runs must be in ACT's state JSON format.

    This command does NOT perform any location mapping.
  |}

let command : Command.t =
  Command.basic ~summary:"compare two state observations" ~readme
    Command.Let_syntax.(
      let%map_open standard_args = Common_cmd.Args.Standard.get
      and human_readable =
        flag "human-readable" no_arg
          ~doc:"If true, output human-readable summary instead of JSON"
      and oracle_raw = anon ("ORACLE_NAME" %: Filename.arg_type)
      and subject_raw = anon ("SUBJECT_NAME" %: Filename.arg_type) in
      fun () ->
        Common_cmd.Common.lift_command standard_args
          ~f:(run ~oracle_raw ~subject_raw ~human_readable))
