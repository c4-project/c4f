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

open Base

module Make (B : Act_backend.Runner_types.Basic) : Act_backend.Runner_types.S =
Act_backend.Runner.Make (struct
  module Unchecked_filter = Filter.Make (B)
  module Reader = Reader

  let make_harness_unchecked (_arch : Act_backend.Arch.t)
      ~(input_path : Fpath.t) ~(output_dir : Fpath.t) :
      string list Or_error.t =
    let prog = Act_backend.Spec.cmd B.spec in
    let output_dir = Fpath.(output_dir / "") in
    let argv =
      [Fpath.to_string input_path; "-o"; Fpath.to_string output_dir]
    in
    Or_error.Let_syntax.(
      let%map () =
        Or_error.tag ~tag:"While running litmus" (B.Runner.run ~prog argv)
      in
      ["make"; "sh ./run.sh"])
end)

let make (module B : Act_backend.Runner_types.Basic) =
  (module Make (B) : Act_backend.Runner_types.S)
