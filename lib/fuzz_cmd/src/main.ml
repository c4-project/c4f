(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

let readme () : string =
  Act_utils.My_string.format_for_readme
    {| The ACT fuzzer adds nonsense to C litmus tests, producing larger
(but hopefully observationally-refining) C litmus tests that exercise more
of a C compiler's control flows.

To run the fuzzer on a single C litmus test, use the `run` subcommand.

To check the list of available fuzzer actions and their currently configured
weights, use the `list-actions` command.
|}

let command : Command.t =
  Command.group ~summary:"the ACT fuzzer" ~readme
    [("run", Run.command); ("list-actions", List_actions.command)]
