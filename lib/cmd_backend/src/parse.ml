(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
open Act_common

let run ?(fqid : Id.t = Id.of_string "herd")
    (args : Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t)
    (_o : Output.t) (cfg : Act_config.Global.t) : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind input = Common_cmd.Args.With_files.infile_source args in
    let%bind output = Common_cmd.Args.With_files.outfile_sink args in
    let%bind (module Backend : Act_backend.Instance_types.S) =
      Common_cmd.Backend_support.lookup_and_resolve_in_cfg fqid ~cfg
    in
    let%bind obs = Backend.Reader.load input in
    Plumbing.Output.with_output output ~f:(fun oc ->
        Or_error.return
          (Yojson.Safe.pretty_to_channel oc
             (Act_state.Observation.yojson_of_t obs))))

let command : Command.t =
  Command.basic ~summary:"parse native output from a configured test backend"
    Command.Let_syntax.(
      let%map standard_args = Common_cmd.Args.(With_files.get Standard.get)
      and sim = Common_cmd.Args.simulator () in
      fun () ->
        Common_cmd.Common.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ?fqid:sim))
