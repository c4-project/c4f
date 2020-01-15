(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

let run (_o : Act_common.Output.t) (_cfg : Act_config.Global.t) :
    unit Or_error.t =
  Or_error.unimplemented "TODO(@MattWindsor91)"

let readme () : string =
  Act_utils.My_string.format_for_readme
    {| Probes a machine (locally by default; optionally a remote machine via
    SSH) for compilers and backends.  If successful, the command prints a
    configuration stanza for the machine to stdout. |}

let command : Core_kernel.Command.t =
  Command.basic ~summary:"probes a machine for ACT configuration" ~readme
    Command.Let_syntax.(
      let%map standard_args = Common_cmd.Args.Standard.get in
      fun () -> Common_cmd.Common.lift_command standard_args ~f:run)
