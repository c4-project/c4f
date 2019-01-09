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

open Core_kernel
open Utils

module Config = struct
  type t =
    { enabled : bool
    ; cmd     : string option
    ; argv    : string list
    }
  [@@deriving sexp, fields, make]
  ;;

  let default () =
    { enabled = true
    ; cmd     = None
    ; argv    = []
    }
  ;;

  let cmd (* override *) t = Option.value ~default:"cpp" (cmd t)
end

module Filter : Filter.S with type aux_i = Config.t
                          and type aux_o = unit =
  Filter.Make_in_file_only (struct
    type aux_i = Config.t
    type aux_o = unit
    let name = "C preprocessor"

    let tmp_file_ext = Fn.const "c"

    let run { Filter.aux; _ } infile (oc : Out_channel.t) =
      let argv = Config.argv aux @ [ Fpath.to_string infile ] in
      Run.Local.run ~oc ~prog:(Config.cmd aux) argv
    ;;
  end)

module Chain_filter (Dest : Utils.Filter.S) :
  Utils.Filter.S with type aux_i = (Config.t * Dest.aux_i)
                  and type aux_o = (unit option * Dest.aux_o) =
  Utils.Filter.Chain_conditional_first (struct
    module First = Filter
    module Second = Dest

    type aux_i_combi = (Config.t * Dest.aux_i)
    let select { Utils.Filter.aux = (cfg, rest); _ } =
      if Config.enabled cfg
      then `Both (cfg, rest)
      else `One  rest
    ;;
  end)
