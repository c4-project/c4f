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
open Stdio
include Filter_intf

let run_direct ?(oc : Out_channel.t = stdout) (cfg : Config.Litmus_tool.t)
    (argv : string list) : unit Or_error.t =
  let prog = Config.Litmus_tool.cmd cfg in
  Or_error.tag ~tag:"While running litmus"
    (Utils.Runner.Local.run ~oc ~prog argv)

module Make (B : Basic) : Sim.Runner.Basic_filter =
Utils.Filter.Make_on_runner (struct
  module Runner = B.Runner

  type aux_i = Sim.Arch.t

  let name = "Litmus tool"

  let tmp_file_ext = Fn.const "txt"

  let prog (_arch : Sim.Arch.t) = Config.Litmus_tool.cmd B.config

  let argv _cfg (path : string) = [path]
end)

(* TODO *)
