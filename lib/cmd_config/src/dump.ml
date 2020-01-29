(* The Automagic Compiler Tormentor Copyright (c) 2018--2020 Matt Windsor and
   contributors - ACT itself is licensed under the MIT License. See the
   LICENSE file in the project root for more information. - ACT is based in
   part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

let run (_o : Act_common.Output.t) (cfg : Act_config.Global.t) :
    unit Or_error.t =
  Fmt.pr "@[<v>%a@]@." Act_config.Reify.pp cfg ;
  Ok ()

let readme () : string =
  Act_utils.My_string.format_for_readme
    {| Dumps the current fuzzer configuration (eg that read from the ACT
      configuration file) to standard output.

      The dumped configuration is pretty-printed and normalised, but does not
      retain any comments. |}

let command : Core_kernel.Command.t =
  Command.basic ~summary:"dumps the current configuration to stdout" ~readme
    Command.Let_syntax.(
      let%map standard_args = Common_cmd.Args.Standard.get in
      fun () ->
        Common_cmd.Args.Standard.lift_command_with_config standard_args
          ~f:run)
