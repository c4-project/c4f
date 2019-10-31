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
Commands for running test backends, as well as processing their results.

In ACT, a 'test backend' is anything that takes a Litmus test and returns
information about its observed behaviour (in the form of state sets).
Backends can be simulators (like Herd7) or execution frameworks (like
Litmus7).
|}

let command : Core_kernel.Command.t =
  Core_kernel.Command.group
    ~summary:"commands for dealing with test backends" ~readme
    [ ("license", Common_cmd.License.command)
    ; ("list", List.command)
    ; ("make-harness", Make_harness.command)
    ; ("parse", Parse.command)
    ; ("run", Run.command) ]
