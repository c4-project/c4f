(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core_kernel
open Lib

let run
    (seed : int option)
    (args : Args.Standard_with_files.t)
    (o : Output.t)
    (_cfg : Config.Act.t)
    : unit Or_error.t
  =
  let open Or_error.Let_syntax in
  let%map _ =
    C.Filters.Litmus.run_from_string_paths
      (C.Filters.Fuzz { seed; o })
      ~infile:(Args.Standard_with_files.infile_raw args)
      ~outfile:(Args.Standard_with_files.outfile_raw args)
  in
  ()
;;

let readme () : string =
  Common.format_for_readme
    {|
`act c fuzz` takes, as input, a C litmus test.  It then performs various
mutations to the litmus test, and outputs the resulting modified test.
|}
;;

let command : Command.t =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Performs fuzzing mutations on a C litmus test"
    ~readme
    [%map_open
      let standard_args = Args.Standard_with_files.get
      and seed =
        flag
          "seed"
          (optional int)
          ~doc:"INT use this integer as the seed to the fuzzer RNG"
      in
      fun () ->
        Common.lift_command_with_files
          standard_args
          ~with_compiler_tests:false
          ~f:(run seed)]
;;
