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
    {|
Commands for querying and manipulating
single C files or litmus tests in a target-independent way.
|}

let command : Command.t =
  Command.group ~summary:"commands for dealing with C files" ~readme
    [ ("compile", Compile.command)
    ; ("delitmus", Delitmus.command)
    ; ("dump-header", Dump_header.command)
    ; ("license", Common_cmd.License.command)
    ; ("replace-header", Replace_header.command) ]
