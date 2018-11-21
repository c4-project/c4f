(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** [Zipper] implements a form of the 'list zipper' data structure.

    A list zipper contains two lists: a 'right list', which represents
   a sequence of data to be processed; and a 'left list', which
   collects the sequence of data already processed in reverse order.
   At any point, the first element in the right list---the
   'cursor'---represents the item currently being processed.

   The usual use case of a zipper is to 'slide' across the right list,
   moving elements onto the left list one by one, and then 'rewind'
   the left list back onto the right for further processing.

    This version of the list zipper contains a few extensions.  First,
   many operations can be parametrised by a monad---this will usually
   be an error monad like [Or_error], but can be anything else (like a
   state transformer).

    Second, items in the zipper can be [mark]ed, attaching a tag to
   them; later on, if the item still exists in the zipper, the zipper
   can be rewound back to the mark using [recall].  *)

(* NOTE: This interface is highly unstable, and _will_ change as we
   change our mind again and again as to the best way of doing this. *)

open Base

(** [t] is the opaque type of zippers. *)
type 'a t [@@deriving sexp]

(** [make ~left ~right] constructs a zipper with left list [left] and
   right list [right].

    These lists go directly into the zipper itself, so [left], if
   non-empty, should be in the reverse order to how it should appear
   when fully rewound. *)
val make : left:'a list -> right:'a list -> 'a t

(** [of_list xs] converts a list [xs] to a fully-rewound zipper.

    It is equivalent to [make] with an empty [left]. *)
val of_list : 'a list -> 'a t

(** [left_list zipper] gets the raw left list of the zipper: all of the
    already-processed items in reverse order. *)
val left_list : 'a t -> 'a list

(** [right_list zipper] gets the right list of the zipper: all of the
    not-yet-processed items in forwards order. *)
val right_list : 'a t -> 'a list

(** [to_list zipper] returns the list of _all_ items in the zipper,
   including those in the left list.

    All items appear in the same order that they would take in the
    right list if the zipper was fully rewound.  In other words,
    the left list appears first (in reverse order), followed by the
    right list (in forwards order).

    To get only the items in the right list, use [right_list]; to get
   only the items in the left list (reversed), use [left_list]. *)
val to_list : 'a t -> 'a list

(** [push zipper ~value] pushes [value] into [zipper] at the cursor.
   The current cursor becomes the second item in the right list, and
   so on. *)
val push : 'a t -> value:'a -> 'a t

(** [pop_opt zipper] returns [None] if [zipper] has no cursor, or
    [Some (a, zipper')] where [a] is [zipper]'s cursor and [zipper']
    is the new zipper formed by removing [a]. *)
val pop_opt : 'a t -> ('a * 'a t) option

(** [peek_opt ?steps zipper] retrieves the cursor value
    without popping it from the zipper.  If the cursor is empty,
    [None] is returned.

    If [steps] is given, it shifts the effective cursor [steps]
    places forwards. *)
val peek_opt : ?steps:int -> 'a t -> 'a option

(** [left_length zipper] gets the length of [zipper]'s left list. *)
val left_length : 'a t -> int
(** [right_length zipper] gets the length of [zipper]'s right list. *)
val right_length : 'a t -> int

(** [On_monad] provides various zipper operations parametrised by
    a monad. *)
module On_monad : functor (M : Monad.S) -> sig
  (** [fold_mapM_head zipper ~f ~init] performs a fold-mapping
      operation over [zipper]'s cursor.  If [zipper]'s cursor
      is empty, [fold_mapM_head] returns [M.return (init, zipper)]. *)
  val fold_mapM_head
    : 'a t
    -> f:('acc -> 'a -> ('acc * 'a option) M.t)
    -> init:'acc
    -> ('acc * 'a t) M.t
  ;;

  (** [popM zipper ~on_empty] behaves like [pop_opt], but executes a
      monadic action [on_empty] instead of returning [None] when the
      cursor is empty. *)
  val popM
    : 'a t
    -> on_empty:('a t -> ('a * 'a t) M.t)
    -> ('a * 'a t) M.t
  ;;

  (** [peekM ?steps zipper ~on_empty] retrieves the cursor value
      without popping it from the zipper.  If the cursor is empty,
      [~on_empty] is executed and returned.

      If [steps] is given, it shifts the effective cursor [steps]
      places forwards. *)
  val peekM
    :  ?steps:int
    -> 'a t
    -> on_empty:('a t -> 'a M.t)
    -> 'a M.t
  ;;

  (** [stepM ?steps zipper ~on_empty] takes one or more steps across
      [zipper].  The number of steps defaults to 1 (forwards), but
      can be given by [steps]; negative numbers step backwards through
      the zipper.  If the number of steps exceeds the bounds of the
      zipper, [on_empty] is executed and returned. *)
  val stepM
    :  ?steps:int
    -> 'a t
    -> on_empty:('a t -> 'a t M.t)
    -> 'a t M.t
  ;;

  (** [foldM_until zipper ~f ~init ~finish] behaves conceptually like
     [List.fold_until], but folds [f] monadically through the
     remaining elements of a zipper.

      [f] receives the current accumulator, current cursor, and zipper
     with cursor popped at each stage.  It can't directly modify the
     zipper mid-fold, but can influence the value of the final zipper
     provided to the [finish] continuation.  At each step, [f] can
     choose to either [`Stop] the fold and return a value directly;
     [`Drop_and_continue], dropping the cursor from the final zipper;
     or [`Replace_and_continue], giving a replacement value for the
     cursor. *)
  val foldM_until
    :  'a t
    -> f:('acc -> 'a -> 'a t -> [ `Stop of 'final
                                | `Drop_and_continue of 'acc
                                | `Replace_and_continue of 'a * 'acc
                                ] M.t)
    -> init:'acc
    -> finish:('acc -> 'a t -> 'final M.t)
    -> 'final M.t
  ;;

  (** [mapM_head zipper ~f ~on_empty] monadically maps [f] across the
      cursor of [zipper].

      [f] returns the new value of the cursor, or [None] to drop
      the cursor from the zipper entirely.

      If [zipper] doesn't have a cursor, [on_empty] is executed and
      returned. *)
  val mapM_head
    :  'a t
    -> f:('a -> 'a option M.t)
    -> on_empty:('a t -> 'a t M.t)
    -> 'a t M.t
  ;;

  (** [markM zipper ~mark] marks the cursor with [mark], and returns
     the marked-up zipper.  If the cursor is empty, [on_empty] is
      executed and returned. *)
  val markM
    : 'a t
    -> mark:int
    -> on_empty:('a t -> 'a t M.t)
    -> 'a t M.t
  ;;

  (** [recallM zipper ~mark ~on_empty] rewinds [zipper] until the
     cursor is on an element previously marked with [mark].

      If [recallM] runs out of left-list to rewind before finding
     [mark], [on_empty] is executed and returned. *)
  val recallM
    : 'a t
    -> mark:int
    -> on_empty:('a t -> 'a t M.t)
    -> 'a t M.t
  ;;
end

(** [map_head zipper ~f] maps [f] across the cursor of [zipper], if it
    exists, and replaces the cursor with the result (or drops it if
    [f] returns [None]. *)
val map_head : 'a t -> f:('a -> 'a option) -> 'a t

(** [pop zipper] behaves as [pop_opt zipper], but returns an error
    if the cursor is empty. *)
val pop : 'a t -> ('a * 'a t) Or_error.t

(** [stepM ?steps zipper ~on_empty] takes one or more steps across
    [zipper].  The number of steps defaults to 1 (forwards), but
    can be given by [steps]; negative numbers step backwards through
    the zipper.  If the number of steps exceeds the bounds of the
    zipper, an error is returned. *)
val step : ?steps:int -> 'a t -> 'a t Or_error.t

(** [fold_until zipper ~f ~init ~finish] behaves conceptually like
   [List.fold_until], but folds [f] through the remaining elements of
   a zipper.

    [f] receives the current accumulator, current cursor, and zipper
   with cursor popped at each stage.  It can't directly modify the
   zipper mid-fold, but can influence the value of the final zipper
   provided to the [finish] continuation.  At each step, [f] can
   choose to either [`Stop] the fold and return a value directly;
   [`Continue] but drop the cursor from the final zipper; or
   [`Replace_and_continue], giving a replacement value for the
   cursor. *)
val fold_until
  :  'a t
  -> f:('acc -> 'a -> 'a t -> [ `Stop of 'final
                              | `Drop_and_continue of 'acc
                              | `Replace_and_continue of 'a * 'acc
                              ])
  -> init:'acc
  -> finish:('acc -> 'a t -> 'final)
  -> 'final
;;

(** [mark zipper ~mark] marks the cursor with [mark], and returns the
   marked-up zipper.  If the cursor is empty, an error is returned. *)
val mark : 'a t -> mark:int -> 'a t Or_error.t

(** [recall zipper ~mark ~on_empty] rewinds [zipper] until the cursor
   is on an element previously marked with [mark].

    If [recall] runs out of left-list to rewind before finding [mark],
   an error is returned. *)
val recall : 'a t -> mark:int -> 'a t Or_error.t
