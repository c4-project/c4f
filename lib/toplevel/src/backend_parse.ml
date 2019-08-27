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
    (args : Args.Standard.t Args.With_files.t) (_o : Output.t)
    (global_cfg : Act_config.Global.t) : unit Or_error.t =
  Or_error.Let_syntax.(
    (* TODO(@MattWindsor91): machine predicates? *)
    let%bind cfg = Act_config.Act.of_global global_cfg in
    let module Res = Sim_support.Make_resolver (struct
      let cfg = cfg
    end) in
    let%bind input = Args.With_files.infile_source args in
    let%bind output = Args.With_files.outfile_sink args in
    let%bind (module Sim) = Res.resolve_single fqid in
    let%bind obs = Sim.Reader.load_from_isrc input in
    Plumbing.Output.with_output output ~f:(fun oc ->
        Or_error.return
          (Yojson.Safe.pretty_to_channel oc
             (Act_state.Observation.yojson_of_t obs))))

let command : Command.t =
  Command.basic
    ~summary:"parses native output from a configured test backend"
    Command.Let_syntax.(
      let%map standard_args = Args.(With_files.get Standard.get)
      and sim = Args.simulator () in
      fun () ->
        Common.lift_command
          (Args.With_files.rest standard_args)
          ~f:(run standard_args ?fqid:sim))
