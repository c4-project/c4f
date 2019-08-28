(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Main entry point.

    This module contains act's main entry point, which multiplexes all of
    the various act sub-programs. *)

(* NOTE:

   The monolithic `act` binary is DEPRECATED. All commands are slowly moving
   out of it into their own binaries, and subject to further subdivision at
   a later date. *)

open Core
open Toplevel

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
`act` is a toolkit for testing C compilers.  It predominantly deals
with concurrency---specifically, checking whether compilers comply
with the C11 memory model with regards to the assembly they emit.
|}

let command =
  Command.group ~summary:"the Automagic Compiler Tormentor" ~readme
    [("asm", Asm_main.command); ("backend", Backend.command)]

let () = Command.run command
