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
    {|
Commands for querying and manipulating
single C files or litmus tests in a target-independent way.
|}

let command : Command.t =
  Command.group ~summary:"commands for dealing with C files" ~readme
    [ ("delitmus", Delitmus.command)
    ; ("dump-header", Dump_header.command)
    ; ("dump-stats", Dump_stats.command)
    ; ("license", Common_cmd.License.command)
    ; ("make-stub", Make_stub.command)
    ; ("modify-header", Modify_header.command)
    ; ("replace-header", Replace_header.command) ]
