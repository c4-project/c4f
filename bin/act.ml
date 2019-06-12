(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Main entry point.

    This module contains act's main entry point, which multiplexes all of
    the various act sub-programs. *)

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
    [ ("c", C_main.command)
    ; ("asm", Asm_main.command)
    ; ("configure", Configure.command)
    ; ("diff-states", Diff_states.command)
    ; ("test", Test.command)
    ; ("tool", Tool.command) ]

let () = Command.run command
