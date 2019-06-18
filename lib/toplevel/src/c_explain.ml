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
module Ac = Act_common

(* TODO(@MattWindsor91): re-enable litmus mode *)

let run (args : Args.Standard_with_files.t)
    _o _cfg =
  let open Or_error.Let_syntax in
  let%bind infile = Args.Standard_with_files.infile_source args in
  let%bind outfile = Args.Standard_with_files.outfile_sink args in
  let (module M) = Act_c.Filters.c_module true in
  let%map _ = M.run Print infile outfile in
  ()

let command : Command.t =
  Command.basic ~summary:"explains act's understanding of a C file"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard_with_files.get
      in
      fun () ->
        Common.lift_command_with_files standard_args
          ~with_compiler_tests:false ~f:run)
