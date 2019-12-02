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
This program provides miscellaneous commands for dealing with the machines
known to ACT.
|}

let command : Command.t =
  Command.group ~summary:"commands for dealing with machines" ~readme
    [("license", Common_cmd.License.command)]
