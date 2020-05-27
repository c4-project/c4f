(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

let command : Command.t =
  Command.group ~summary:"commands for dealing with ACT configuration"
    [ ("dump", Dump.command)
    ; ("license", Common_cmd.License.command)
    ; ("list-predicates", List_predicates.command) ]
