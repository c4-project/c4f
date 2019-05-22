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
module Shc = Act_sim_herdtools_common

(* Map over an optional tuple. *)
module O2 = Travesty.Bi_mappable.Chain_Bi2_Map1 (Tx.Tuple2) (Option)

(* In the histogram, *> represents a postcondition-satisfying state, and :>
   any other state. *)

let strip_with_separator : string -> string =
  String.strip ~drop:(Tx.Fn.disj Char.is_whitespace (String.mem ":*>"))

let split_line_to_string_tuple (line : string) : string option * string =
  line |> String.lsplit2 ~on:'>'
  |> O2.bi_map
       ~left:(Fn.compose Option.return strip_with_separator)
       ~right:String.strip
  |> Option.value ~default:(None, line)

let parse_int (s : string) : int Or_error.t =
  Or_error.try_with (fun () -> Int.of_string s)

let parse_int_opt : string option -> int option Or_error.t =
  Tx.Option.With_errors.map_m ~f:parse_int

include Shc.Reader.Make (struct
  let try_parse_state_count (line : string) : int option =
    Option.try_with (fun () ->
        Caml.Scanf.sscanf line "Histogram (%d states)" Fn.id )

  let try_split_state_line (line : string) :
      Shc.Reader.state_line Or_error.t =
    let occurrences_str, rest = split_line_to_string_tuple line in
    Or_error.Let_syntax.(
      let%map occurrences = parse_int_opt occurrences_str in
      {Shc.Reader.occurrences; rest})
end)
