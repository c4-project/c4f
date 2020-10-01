(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

open struct
  module Ac = Act_common
end

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
      Reads the current config file, and outputs information about each
      tunable fuzzer parameter, including its computed value
      after taking the config into consideration.

      If verbose mode is enabled, the information is a detailed summary of the
      parameter, including its documentation; otherwise, this command prints
      only the name and current value.
    |}

let summaries_of_config (cfg : Act_config.Global.t) :
    Act_fuzz_run.Config.Param_summary.t Or_error.t =
  cfg |> Act_config.Global.fuzz
  |> Act_fuzz_run.Config.Param_summary.summarise

let print (o : Ac.Output.t) (map : Act_fuzz_run.Config.Param_summary.t) :
    unit =
  let pp =
    Act_fuzz_run.Config.Param_summary.(
      if Ac.Output.is_verbose o then pp else pp_terse)
  in
  Fmt.pr "@[<v>%a@]@." pp map

let run (o : Ac.Output.t) (cfg : Act_config.Global.t) : unit Or_error.t =
  Or_error.(cfg |> summaries_of_config >>| print o)

let command : Command.t =
  Command.basic ~summary:"output the current fuzzer param map" ~readme
    Command.Let_syntax.(
      let%map standard_args = Common_cmd.Args.Standard.get in
      fun () ->
        Common_cmd.Args.Standard.lift_command_with_config standard_args
          ~f:run)
