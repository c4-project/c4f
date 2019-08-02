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

let fuzz_config (cfg : Act_config.Act.t) : Act_config.Fuzz.t =
  cfg |> Act_config.Act.fuzz
  |> Option.value ~default:(Act_config.Fuzz.make ())

let pp_fuzz_summaries : Act_fuzz.Action.Summary.t Id.Map.t Fmt.t =
  Id.pp_map Act_fuzz.Action.Summary.pp

let run_list_fuzzer_actions (_o : Output.t) (cfg : Act_config.Act.t) :
    unit Or_error.t =
  Or_error.(
    cfg |> fuzz_config |> Act_fuzz.Config.summarise
    >>| Fmt.pr "@[<v>%a@]@." pp_fuzz_summaries)

let command : Command.t =
  Command.basic ~summary:"outputs the current fuzzer weight table"
    ~readme:list_fuzzer_actions_readme
    Command.Let_syntax.(
      let%map standard_args = Toplevel.Args.Standard.get in
      fun () ->
        Toplevel.Common.lift_command standard_args
          ~with_compiler_tests:false ~f:run_list_fuzzer_actions)
