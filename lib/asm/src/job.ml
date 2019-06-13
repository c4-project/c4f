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
module Tx = Travesty_base_exts

type 'cfg t =
  { config: 'cfg option
  ; passes: Set.M(Act_sanitiser.Pass_group).t
        [@default Act_sanitiser.Pass_group.standard]
  ; aux: Act_delitmus.Output.Aux.t [@default Act_delitmus.Output.Aux.empty]
  }
[@@deriving make, fields]

let map_m_config (job : 'a t) ~(f : 'a -> 'b Or_error.t) : 'b t Or_error.t =
  Or_error.Let_syntax.(
    let%map config = Tx.Option.With_errors.map_m job.config ~f in
    {job with config})

let symbols (job : _ t) : string list =
  Act_delitmus.Output.Aux.symbols (aux job)

module Output = struct
  type t = {symbol_map: (string, string) List.Assoc.t; warn: unit Fmt.t}
  [@@deriving fields]

  (* Overriding to get the right order. *)
  let warn (f : Formatter.t) (o : t) : unit = warn o f ()

  let emit_warnings (pp_warning : 'a Fmt.t) (iname : string) :
      'a list -> unit Fmt.t = function
    | [] ->
        Fmt.nop
    | ws ->
        let pp f w = Fmt.pf f "@[<h>-@ @[<hov>%a@]@]@," pp_warning w in
        fun f () ->
          Fmt.pf f "Warnings@ for@ %s:@ @[<v>%a@]@." iname
            (fun f -> List.iter ~f:(pp f))
            ws

  let make (pp_warning : 'a Fmt.t) iname
      (symbol_map : (string, string) List.Assoc.t) warnings : t =
    {symbol_map; warn= emit_warnings pp_warning iname warnings}
end
