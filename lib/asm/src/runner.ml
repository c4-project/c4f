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

open Core_kernel
module Tx = Travesty_core_kernel_exts
include Runner_intf

module Make (R : Runnable) : S with type cfg = R.cfg = struct
  type cfg = R.cfg

  let parse isrc inp =
    let iname = Act_utils.Io.In_source.to_string isrc in
    Or_error.tag_arg
      (R.parse_asm iname isrc inp)
      "Error while parsing assembly" iname String.sexp_of_t

  let unstringify_symbol (sym : string) : R.Symbol.t Or_error.t =
    Result.of_option
      (R.Symbol.of_string_opt sym)
      ~error:
        (Error.create_s
           [%message "Symbol can't be converted from string" ~symbol:sym])

  let unstringify_symbols : string list -> R.Symbol.t list Or_error.t =
    Tx.Or_error.combine_map ~f:unstringify_symbol

  let in_source_to_basename (is : Act_utils.Io.In_source.t) : string =
    is |> Act_utils.Io.In_source.to_file
    |> Option.value_map
         ~f:(fun fn -> Fpath.(fn |> rem_ext |> basename))
         ~default:"stdin"

  include Act_utils.Filter.Make (struct
    type aux_i = R.cfg Job.t

    type aux_o = Job.Output.t

    let name = R.name

    let tmp_file_ext _ = R.tmp_file_ext

    let run
        ({Act_utils.Filter.aux; src; sink} :
          R.cfg Job.t Act_utils.Filter.ctx) ic oc : Job.Output.t Or_error.t
        =
      let in_name = in_source_to_basename src in
      Or_error.Let_syntax.(
        let%bind program = parse src ic in
        let%bind symbols = unstringify_symbols (Job.symbols aux) in
        let config =
          Tx.Option.value_f (Job.config aux) ~default_f:R.default_config
        in
        let passes = Job.passes aux in
        R.run ~in_name ~program ~symbols ~config ~passes sink oc)
  end)
end
