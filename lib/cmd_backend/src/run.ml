(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
open Act_common

let run ?(arch = Act_backend.Arch.c) ?(fqid : Id.t = Id.of_string "herd")
    (args : Common_cmd.Args.Standard.t Common_cmd.Args.With_files.t)
    (_o : Output.t) (cfg : Act_config.Global.t) : unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind (module Backend : Act_backend.Instance_types.S) =
      Common_cmd.Backend_support.lookup_and_resolve_in_cfg fqid ~cfg
    in
    Common_cmd.Args.With_files.run_filter
      (module Backend.Filter)
      args ~aux_in:arch)

let command : Command.t =
  Command.basic ~summary:"run a configured test backend"
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get Standard.get)
      and backend = Common_cmd.Args.backend ()
      and arch = Common_cmd.Args.backend_arch in
      fun () ->
        Common_cmd.Common.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run standard_args ?arch ?fqid:backend))
