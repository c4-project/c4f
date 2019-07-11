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

let find_at_most_one (type a b) ?(item_name : string = "item")
    (items : a list) ~(f : a -> b option) ~(on_empty : b Or_error.t) :
    b Or_error.t =
  Or_error.(
    match List.filter_map items ~f with
    | [] ->
        on_empty
    | [x] ->
        return x
    | _ ->
        errorf "Duplicate %s" item_name)

let find_one_opt (type a b) ?(item_name : string = "item") (items : a list)
    ~(f : a -> b option) : b option Or_error.t =
  let f (x : a) : b option option =
    match f x with Some x -> Some (Some x) | None -> None
  in
  find_at_most_one items ~item_name ~f ~on_empty:(Or_error.return None)

let find_one (type a b) ?(item_name : string = "item") (items : a list)
    ~(f : a -> b option) : b Or_error.t =
  find_at_most_one items ~item_name ~f
    ~on_empty:(Or_error.errorf "Expected at least one %s" item_name)

let random_index (xs : 'a list) ~(random : Splittable_random.State.t) :
    int option =
  if List.is_empty xs then None
  else Some (Splittable_random.int random ~lo:0 ~hi:(List.length xs - 1))

let random_item (xs : 'a list) ~(random : Splittable_random.State.t) :
    'a option =
  Option.(random_index ~random xs >>= List.nth xs)
