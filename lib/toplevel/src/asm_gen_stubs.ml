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

open Core_kernel
module A = Act_common

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
The `asm gen-stubs` command generates GCC assembly directives that can
be slotted into a simulation harness.
|}

module In = Asm_common.Input

let run (input : In.t) : unit Or_error.t =
  ignore input ;
  Or_error.unimplemented "TODO"

let command : Command.t =
  Command.basic ~summary:"generates GCC asm stubs from an assembly file"
    ~readme
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard_asm.get in
      fun () -> Asm_common.lift_command standard_args ~f:run ~default_passes:Act_config.Sanitiser_pass.explain)
