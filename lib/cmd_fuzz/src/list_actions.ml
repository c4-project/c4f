(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core (* for Filename.arg_type *)

open Act_common

let list_fuzzer_actions_readme () : string =
  Act_utils.My_string.format_for_readme
    {|
      Reads the current config file, and outputs information about each
      fuzzer action, including its computed weight after taking the config
      into consideration.
    |}

let run_list_fuzzer_actions (_o : Output.t) (cfg : Act_config.Global.t) :
    unit Or_error.t =
  Or_error.(
    cfg |> Act_config.Global.fuzz |> Act_fuzz.Config.summarise
    >>| Fmt.pr "@[<v>%a@]@." Act_fuzz.Action.Summary.pp_map)

let command : Command.t =
  Command.basic ~summary:"output the current fuzzer weight table"
    ~readme:list_fuzzer_actions_readme
    Command.Let_syntax.(
      let%map standard_args = Common_cmd.Args.Standard.get in
      fun () ->
        Common_cmd.Args.Standard.lift_command_with_config standard_args
          ~f:run_list_fuzzer_actions)
