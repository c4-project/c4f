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

(** Tests the output of the 'act asm explain' command. *)

open Core
module Ac = Act_common

let run (dir : Fpath.t) : unit Or_error.t =
  let arch = Ac.Id.of_string "x86.att" in
  Or_error.Let_syntax.(
    let%bind (module L) =
      Toplevel.Language_support.asm_runner_from_arch arch
    in
    let module Exp = Act_asm.Explainer.Make (L) in
    Common.regress_run_asm_many
      (module Exp.Filter)
      "Explainer" Act_sanitiser.Pass_group.explain dir
      ~config_fn:(fun aux -> Act_asm.Explainer.Config.make ~aux ()) 
  )

let command =
  Common.make_regress_command ~summary:"runs explainer regressions" run

let () = Command.run command
