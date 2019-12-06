(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
Commands for querying and invoking the C compilers known to ACT.
|}

let command : Core_kernel.Command.t =
  Core_kernel.Command.group ~summary:"commands for dealing with compilers"
    ~readme
    [ ("license", Common_cmd.License.command)
    ; ("list", List.command)
    ; ("run", Run.command) ]
