(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Base
open Stdio
open Utils

module Config = struct
  type t =
    { cmd        : string [@default "litmus7"] [@drop_if_default]
    } [@@deriving sexp, make]
  ;;

  let pp f { cmd } =
    Fmt.pf f "litmus (%s)" cmd
  ;;

  let create = make
end

module Filter : Filter.S with type aux_i = Config.t
                          and type aux_o = unit =
  Filter.Make_in_file_only (struct
    type aux_i = Config.t
    type aux_o = unit

    let run (config : Config.t) (path : Fpath.t)
        (_sink : Io.Out_sink.t) (oc : Out_channel.t)
      : unit Or_error.t =
      let prog = config.cmd in
      Or_error.tag ~tag:"While running litmus"
        (Run.Local.run ~oc ~prog [ Fpath.to_string path ])
  end)

(* TODO *)
