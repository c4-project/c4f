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

(** Utility functions for lists. *)

open Base

(** {2 Finding and transforming specific amounts of items} *)

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

(** {2 Using a splittable RNG to access elements} *)

val random_index : _ list -> random:Splittable_random.State.t -> int option
(** [random_index xs ~random] returns [None] if [xs] is empty, and [Some i],
    where [i] is a random number generated with [random] between [0] and
    [length xs] exclusive, otherwise. *)

val random_stride :
  _ list -> random:Splittable_random.State.t -> (int * int) option
(** [random_index xs ~random] returns [None] if [xs] is empty, and
    [Some (i, n)], where [i] is a random index generated with [random]
    between [0] and [length xs] exclusive and [n] is a random length
    generated with [random] between [0] and the [length] of the sub-list
    after [i], otherwise. *)

val random_item : 'a list -> random:Splittable_random.State.t -> 'a option
(** [random_item xs ~random] behaves like
    {{!List.random_element} List.random_element}, but uses a splittable RNG
    for compatibility with Quickcheck etc. *)

(** {2 Manipulating lists} *)

val split_or_error : 'a list -> int -> ('a list * 'a list) Or_error.t
(** [split_or_error xs n] behaves like [split xs n], but returns an error if
    [n] is negative or greater than the length of the list. *)

val splice :
     'a list
  -> pos:int
  -> len:int
  -> replace_f:('a list -> 'a list)
  -> 'a list Or_error.t
(** [splice xs ~pos ~len ~replace_f] replaces the part of [xs] denoted by
    [pos] and [len] with its image in [replace_f]. It returns an error if
    [pos] is out of bounds, or the list from [pos] onwards is shorter than
    [len]. *)

val map_sub :
  'a list -> pos:int -> len:int -> f:('a -> 'a) -> 'a list Or_error.t
(** [map_sub xs ~pos ~len ~f] maps [f] over the part of [xs] denoted by
    [pos] and [len]. *)
