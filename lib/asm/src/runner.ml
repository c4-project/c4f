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

module Make (R : Runner_intf.Runnable) :
  Runner_intf.S with type cfg = R.cfg = struct
  type cfg = R.cfg

  let parse isrc =
    let iname = Plumbing.Input.to_string isrc in
    Or_error.tag_arg
      (R.Program.load_from_isrc isrc)
      "Error while parsing assembly" iname String.sexp_of_t

  let in_source_to_basename (is : Plumbing.Input.t) : string =
    is |> Plumbing.Input.to_file
    |> Option.value_map
         ~f:(fun fn -> Fpath.(fn |> rem_ext |> basename))
         ~default:"stdin"

  include Plumbing.Filter.Make (struct
    type aux_i = R.cfg Job.t

    type aux_o = Job.Output.t

    let name = R.name

    let run (ctx : R.cfg Job.t Plumbing.Filter_context.t) _ oc :
        Job.Output.t Or_error.t =
      let aux = Plumbing.Filter_context.aux ctx in
      let input = Plumbing.Filter_context.input ctx in
      let in_name = in_source_to_basename input in
      Or_error.Let_syntax.(
        let%bind program = parse input in
        let config =
          Tx.Option.value_f (Job.config aux) ~default_f:R.default_config
        in
        let passes = Job.passes aux in
        R.run ~in_name ~program ~config ~passes oc)
  end)
end
