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

(** Tests the output of the 'act c delitmus' command. *)

open Core

let parse_config_failures ~(file : Fpath.t) ~(path : Fpath.t) :
    unit Or_error.t =
  ignore file ;
  let r = Act_config.Frontend.load ~path in
  Fmt.(pr "@[%a@]@." (result ~ok:(always "No failures.") ~error:Error.pp)) r ;
  Result.ok_unit

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "config" / "failures" / "") in
  Common.regress_on_files "Config parser (failures)" ~dir:full_dir
    ~ext:"conf" ~f:parse_config_failures

let command =
  Common.make_regress_command
    ~summary:"runs config parser failure regressions" run

let () = Command.run command
