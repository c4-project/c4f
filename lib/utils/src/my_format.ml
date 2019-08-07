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

let null_formatter () =
  Caml.Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ())

let pp_c_braces (pi : 'a Fmt.t) : 'a Fmt.t =
  Fmt.(hvbox (hvbox ~indent:4 (any "{@ " ++ pi) ++ any "@ }"))

let pp_kv f k pv v =
  Fmt.(hvbox ~indent:1 (pair ~sep:(const (char ++ sp) ':') string pv))
    f (k, v)

let pp_set (type elem cmp) (elem : elem Fmt.t) : (elem, cmp) Set.t Fmt.t =
  Fmt.(using Set.to_list (braces (box (list ~sep:comma elem))))

let pp_if (t : unit Fmt.t) (f : unit Fmt.t) : bool Fmt.t =
 fun fmt x -> (if x then t else f) fmt ()
