(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core
open Act_common

let run (_o : Output.t) (_cfg : Act_config.Act.t) ~(oracle_raw : string)
    ~(subject_raw : string) : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind oracle_in = Plumbing.Input.of_string_opt (Some oracle_raw) in
    let%bind oracle = Act_state.Observation.load_from_isrc oracle_in in
    let%bind subject_in = Plumbing.Input.of_string_opt (Some subject_raw) in
    let%bind subject = Act_state.Observation.load_from_isrc subject_in in
    let%map diff = Act_state.Diff.run ~oracle ~subject in
    Act_state.Diff.pp Fmt.stdout diff)

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
  Command.basic ~summary:"compares two state observations" ~readme
    Command.Let_syntax.(
      let%map_open standard_args = ignore anon ; Toplevel.Args.Standard.get
      and oracle_raw = anon ("ORACLE_NAME" %: Filename.arg_type)
      and subject_raw = anon ("SUBJECT_NAME" %: Filename.arg_type)
      in
      fun () ->
        Toplevel.Common.lift_command standard_args
          ~with_compiler_tests:false
          ~f:(run ~oracle_raw ~subject_raw))
