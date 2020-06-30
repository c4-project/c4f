(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

open Base

let of_string (s : string) : Fpath.t Or_error.t =
  Result.map_error (Fpath.of_string s) ~f:(function `Msg s ->
      Error.of_string s )

let lift_str (s : string option) ~(f : Fpath.t -> 'a) ~(default : 'a) :
    'a Or_error.t =
  match s with
  | None ->
      Or_error.return default
  | Some s ->
      Or_error.(s |> of_string >>| f)

let of_string_option : string option -> Fpath.t option Or_error.t =
  lift_str ~f:Option.some ~default:None

let filename_no_ext (f : Fpath.t) : string =
  Fpath.(filename (rem_ext ~multi:true f))
