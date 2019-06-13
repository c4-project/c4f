(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel

let run (args : Args.Standard_with_files.t) _o _cfg =
  Or_error.Let_syntax.(
    let%bind input = Args.Standard_with_files.infile_source args in
    let%bind output = Args.Standard_with_files.outfile_sink args in
    Or_error.ignore_m (Act_delitmus.Filter.run () input output))

let command : Command.t =
  Command.basic ~summary:"converts a C litmus test to a normal C file"
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard_with_files.get in
      fun () ->
        Common.lift_command_with_files standard_args
          ~with_compiler_tests:false ~f:run)
