(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open struct
  module Af = Act_fuzz
end

open Core_kernel

let store_trace (trace : Af.Trace.t) : Fpath.t Or_error.t =
  let tmpfile = Plumbing.Output.temp_file ~prefix:"trace" ~ext:"txt" in
  Plumbing.Output.with_output (Plumbing.Output.file tmpfile) ~f:(fun f ->
      Sexp.output_mach f (Af.Trace.sexp_of_t trace) ;
      Ok tmpfile)

let run_with_trace (trace : Af.Trace.t) ~(cmd : string) ~(argv : string list)
    : bool =
  match store_trace trace with
  | Ok name ->
      Shell.test cmd (Fpath.to_string name :: argv)
  | Error e ->
      Stdio.eprint_s (Error.sexp_of_t e) ;
      false

let bisector (o : Act_common.Output.t) (trace : Af.Trace.t) ~(cmd : string)
    ~(argv : string list) : [`Bad | `Good] =
  if run_with_trace trace ~cmd ~argv then (
    Act_common.Output.pv o "%d GOOD\n" (Af.Trace.length trace) ;
    `Good )
  else (
    Act_common.Output.pv o "%d BAD\n" (Af.Trace.length trace) ;
    `Bad )

let bisect (o : Act_common.Output.t) (trace : Af.Trace.t) ~(cmd : string)
    ~(argv : string list) : Af.Trace.t =
  Act_fuzz.Trace.bisect trace ~f:(bisector o ~cmd ~argv)

let run ~(cmd : string) ~(argv : string list)
    (args : _ Common_cmd.Args.With_files.t) (o : Act_common.Output.t) :
    unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind trace_in = Common_cmd.Args.With_files.infile_source args in
    let%map trace = Act_fuzz.Trace.load trace_in in
    let trace' = bisect o trace ~cmd ~argv in
    Stdio.print_s (Af.Trace.sexp_of_t trace'))

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
      Reads in the given trace file and bisects it sending it as standard input
      to the given command and presuming that exit code 0 means the trace is good
    and code 1 bad.
    |}

let command : Command.t =
  Command.basic ~summary:"replay a fuzzing trace on a C litmus test" ~readme
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get Standard.get)
      and cmd =
        flag "command" (required string)
          ~doc:"NAME the name of the command to run on intermediate traces"
      and argv =
        flag "arg" (listed string)
          ~doc:"ARG an argument to pass to the named command"
      in
      fun () ->
        Common_cmd.Args.Standard.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run ~cmd ~argv standard_args))
