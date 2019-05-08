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

open Core
open Act_common

let run (_o : Output.t) (_cfg : Config.Act.t)
    ~(oracle_raw : [< `Herd of string | `Litmus of string])
    ~(subject_raw : [< `Herd of string | `Litmus of string]) :
    unit Or_error.t =
  ignore oracle_raw ;
  ignore subject_raw ;
  Or_error.unimplemented "TBC"

let herd_flag (name : string) : [> `Herd of string] option Command.Param.t =
  Command.(
    Param.map
      ~f:(Option.map ~f:(fun x -> `Herd x))
      (Param.flag (name ^ "-herd")
         (Flag.optional Filename.arg_type)
         ~doc:("FILENAME read Herd7 output for " ^ name ^ " from this file")))

let litmus_flag (name : string) :
    [> `Litmus of string] option Command.Param.t =
  Command.(
    Param.map
      ~f:(Option.map ~f:(fun x -> `Litmus x))
      (Param.flag (name ^ "-litmus")
         (Flag.optional Filename.arg_type)
         ~doc:
           ("FILENAME read Litmus7 output for " ^ name ^ " from this file")))

let file_flag (name : string) :
    [> `Herd of string | `Litmus of string] Command.Param.t =
  Command.(
    Param.choose_one
      [herd_flag name; litmus_flag name]
      ~if_nothing_chosen:`Raise)

let readme () =
  Utils.My_string.format_for_readme
    {|
    `act diff-states` takes two summaries of simulation runs: an
    'oracle' (usually a program _before_ compilation), and a
    'subject' (usually the same program _after_ compilation).  It
    then parses the summaries, applies any provided variable name
    mappings, and compares the sets of final states on both ends.
  |}

let command : Command.t =
  Command.basic ~summary:"compares two simulation runs" ~readme
    Command.Let_syntax.(
      let%map_open standard_args = Args.Standard.get
      and oracle_raw = file_flag "oracle"
      and subject_raw = file_flag "subject" in
      fun () ->
        Common.lift_command standard_args ~with_compiler_tests:false
          ~f:(fun _args -> run ~oracle_raw ~subject_raw))
