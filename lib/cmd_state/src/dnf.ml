(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core
open Act_common


let run (args : Toplevel.Args.Standard.t Toplevel.Args.With_files.t)
    (_o : Output.t) (_cfg : Act_config.Global.t) : unit Or_error.t =
  Toplevel.Args.With_files.run_filter (module Act_state.Dnf.Filter) args
    ~aux_in:()


let readme () =
  Act_utils.My_string.format_for_readme
    {|
    This command takes an observation and generates a for-all Litmus
    postcondition
    that witnesses each state originally
    witnessed by the observation.
  |}

let command : Command.t =
  Command.basic ~summary:"generate postcondition from observation" ~readme
    Command.Let_syntax.(
      let%map_open args = Toplevel.Args.With_files.get (Toplevel.Args.Standard.get) in
      fun () ->
        Toplevel.Common.lift_command (Toplevel.Args.With_files.rest args)
          ~f:(run args))
