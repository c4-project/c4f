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

val find_at_most_one :
     ?item_name:string
  -> 'a list
  -> f:('a -> 'b option)
  -> on_empty:'b Or_error.t
  -> 'b Or_error.t
(** [find_at_most_one ?item_name items ~f ~on_empty] tries to find an item
    in [items] for which [f] returns [Some]. It returns the result if
    precisely one exists; [on_empty] if none exist, and an error otherwise. *)

val find_one_opt :
     ?item_name:string
  -> 'a list
  -> f:('a -> 'b option)
  -> 'b option Or_error.t
(** [find_one_opt ?item_name items ~f] tries to find an item in [items] for
    which [f] returns [Some]. It returns the result in [Some] if precisely
    one exists, [None] if none exist, and an error otherwise. *)

val find_one :
  ?item_name:string -> 'a list -> f:('a -> 'b option) -> 'b Or_error.t
(** [find_one ?item_name items ~f] tries to find an item in [items] for
    which [f] returns [Some]. It returns the result if precisely one exists,
    and an error otherwise. *)
