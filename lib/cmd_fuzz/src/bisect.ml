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
    ~(argv : string list) : [`Left | `Right] =
  if run_with_trace trace ~cmd ~argv then (
    Act_common.Output.pv o "%d <-\n" (Af.Trace.length trace) ;
    `Left )
  else (
    Act_common.Output.pv o "%d ->\n" (Af.Trace.length trace) ;
    `Right )

let bisect (o : Act_common.Output.t) (trace : Af.Trace.t) ~(cmd : string)
    ~(argv : string list) ~(want : [`Last_on_left | `First_on_right]) :
    Af.Trace.t =
  Act_fuzz.Trace.bisect trace ~f:(bisector o ~cmd ~argv) ~want

let run ~(cmd : string) ~(argv : string list)
    ?(want : [`Last_on_left | `First_on_right] = `First_on_right)
    (args : _ Common_cmd.Args.With_files.t) (o : Act_common.Output.t) :
    unit Or_error.t =
  Or_error.Let_syntax.(
    let%bind dest = Common_cmd.Args.With_files.outfile_sink args in
    let%bind trace_in = Common_cmd.Args.With_files.infile_source args in
    let%bind trace = Act_fuzz.Trace.load trace_in in
    let trace' = bisect o trace ~cmd ~argv ~want in
    Af.Trace.store trace' ~dest)

let readme () : string =
  Act_utils.My_string.format_for_readme
    {|
      Reads in the given trace file and bisects it sending it as standard input
      to the given command and presuming that success means the trace
      should shrink (move leftwards) and failure means it should grow
      (move rightwards).
    |}

let command : Command.t =
  Command.basic ~summary:"replay a fuzzing trace on a C litmus test" ~readme
    Command.Let_syntax.(
      let%map_open standard_args =
        Common_cmd.Args.(With_files.get Standard.get)
      and want =
        choose_one
          Common_cmd.Args.
            [ flag_to_enum_choice `Last_on_left "-last-left"
                ~doc:
                  "Return the largest trace for which the oracle returned \
                   'left' (success)"
            ; flag_to_enum_choice `First_on_right "-first-right"
                ~doc:
                  "Return the small trace for which the oracle returned \
                   'right' (failure)" ]
          ~if_nothing_chosen:Return_none
      and cmd =
        flag "-command" (required string)
          ~doc:"NAME the name of the command to run on intermediate traces"
      and argv =
        flag "--"
          (map_flag ~f:(Option.value ~default:[]) escape)
          ~doc:"ARGS all further arguments are passed to the oracle"
      in
      fun () ->
        Common_cmd.Args.Standard.lift_command
          (Common_cmd.Args.With_files.rest standard_args)
          ~f:(run ~cmd ~argv ?want standard_args))
