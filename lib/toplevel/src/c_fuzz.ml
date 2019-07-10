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
open Act_common

let run (seed : int option) (args : Args.Standard_with_files.t)
    (o : Output.t) (act_config : Act_config.Act.t) : unit Or_error.t =
  let config =
    act_config |> Act_config.Act.fuzz
    |> Option.value ~default:(Act_config.Fuzz.make ())
  in
  Or_error.Let_syntax.(
    let%bind input = Args.Standard_with_files.infile_source args in
    let%bind output = Args.Standard_with_files.outfile_sink args in
    Or_error.ignore_m (Act_c.Filter.run {seed; o; config} input output))

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
`act c fuzz` takes, as input, a C litmus test.  It then performs various
mutations to the litmus test, and outputs the resulting modified test.
|}

let command : Command.t =
  Command.basic ~summary:"performs fuzzing mutations on a C litmus test"
    ~readme
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard_with_files.get
      and seed =
        flag "seed" (optional int)
          ~doc:"INT use this integer as the seed to the fuzzer RNG"
      in
      fun () ->
        Common.lift_command_with_files standard_args
          ~with_compiler_tests:false ~f:(run seed))
