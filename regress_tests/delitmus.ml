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
module Ac = Act_common

let pp_aux : Act_delitmus.Aux.t Fmt.t =
  Fmt.(
    using Act_delitmus.Aux.to_yojson (Yojson.Safe.pretty_print ~std:false)
  )

let print_aux : Act_delitmus.Aux.t -> unit =
  Fmt.pr "@[<v>@ /* --Begin auxiliary output--@ @ %a@ @ --End auxiliary output-- */@]@." pp_aux

let delitmus_file ~(file : Fpath.t) ~(path : Fpath.t) : unit Or_error.t =
  ignore file ;
  Or_error.Let_syntax.(
    let%map output =
      Act_delitmus.Filter.run ()
        (Plumbing.Input.of_fpath path)
        Plumbing.Output.stdout
    in
    print_aux output)

let run (test_dir : Fpath.t) : unit Or_error.t =
  let full_dir = Fpath.(test_dir / "litmus" / "") in
  Common.regress_on_files "Delitmus" ~dir:full_dir ~ext:"litmus"
    ~f:delitmus_file

let command =
  Common.make_regress_command ~summary:"runs delitmusifier regressions" run

let () = Command.run command
