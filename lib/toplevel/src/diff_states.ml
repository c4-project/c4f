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

let run (_o : Output.t) (_cfg : Act_config.Act.t) ~(oracle_raw : string)
    ~(subject_raw : string) : unit Or_error.t =
  ignore oracle_raw ;
  ignore subject_raw ;
  Or_error.unimplemented "TBC"

let readme () =
  Act_utils.My_string.format_for_readme
    {|
    `act diff-states` takes two summaries of backend runs: an
    'oracle' (usually a program _before_ compilation), and a
    'subject' (usually the same program _after_ compilation).  It
    then parses the summaries, applies any provided variable name
    mappings, and compares the sets of final states on both ends.

    Both runs must be in ACT's state JSON format.
  |}

let command : Command.t =
  Command.basic ~summary:"compares two simulation runs" ~readme
    Command.Let_syntax.(
      let%map_open standard_args = ignore anon ; Args.Standard.get
      and oracle_raw = anon ("ORACLE_NAME" %: Filename.arg_type)
      and subject_raw = anon ("SUBJECT_NAME" %: Filename.arg_type) in
      fun () ->
        Common.lift_command standard_args ~with_compiler_tests:false
          ~f:(run ~oracle_raw ~subject_raw))
