(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core

let readme () : string =
  C4f_utils.My_string.format_for_readme
    {| The ACT fuzzer adds nonsense to C litmus tests, producing larger
(but hopefully observationally-refining) C litmus tests that exercise more
of a C compiler's control flows.

To run the fuzzer on a single C litmus test, use the `run` subcommand.

To check the list of available fuzzer actions and their currently configured
weights, use the `list-actions` command.
|}

let command : Command.t =
  Command.group ~summary:"the ACT fuzzer" ~readme
    [ ("bisect", Bisect.command)
    ; ("license", Common_cmd.License.command)
    ; ("replay", Replay.command)
    ; ("run", Run.command)
    ; ("list-actions", List_actions.command)
    ; ("list-params", List_params.command) ]
