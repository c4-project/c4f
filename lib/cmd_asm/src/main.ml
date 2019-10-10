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
The `asm` command group contains commands for querying and manipulating
single assembly files or litmus tests.

Some of the commands also generalise to single C files or litmus tests, by
passing them through a nominated compiler and, if necessary, `act`'s
delitmusifier.  For target-independent operations on single C files/tests,
see the `c` command group.
|}

let command : Command.t =
  Command.group ~summary:"commands for dealing with assembly files" ~readme
    [ ("explain", Explain.command)
    ; ("litmusify", Litmusify.command)
    ; ("license", Common_cmd.License.command) ]
