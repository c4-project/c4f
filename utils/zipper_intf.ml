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

open Core_kernel

(** [S_non_monadic] contains the core operations of a zipper,
    without any parametrisation over a particular failure monad. *)
module type S_non_monadic = sig
  (** The opaque type of zippers. *)
  type 'a t [@@deriving sexp]

  (** {3 Construction and destruction} *)

  (** [make ~left ~right] constructs a zipper with left list [left] and
      right list [right].

      These lists go directly into the zipper itself, so [left], if
      non-empty, should be in the reverse order to how it should appear
      when fully rewound. *)
  val make : left:'a list -> right:'a list -> 'a t

  (** [of_list xs] converts a list [xs] to a fully-rewound zipper.

    It is equivalent to [make] with an empty [left]. *)
  val of_list : 'a list -> 'a t

  (** [to_list zipper] returns the list of _all_ items in the zipper,
     including those in the left list.

      All items appear in the same order that they would take in the
     right list if the zipper was fully rewound.  In other words, the
     left list appears first (in reverse order), followed by the right
     list (in forwards order).

      To get only the items in the right list, use [right_list]; to
     get only the items in the left list (reversed), use
     [left_list]. *)
  val to_list : 'a t -> 'a list

  (** {3 Querying the left and right lists} *)

  (** [left_list zipper] gets the raw left list of the zipper: all of the
      already-processed items in reverse order. *)
  val left_list : 'a t -> 'a list

  (** [right_list zipper] gets the right list of the zipper: all of the
      not-yet-processed items in forwards order. *)
  val right_list : 'a t -> 'a list

  (** [to_two_lists zipper] is [(left_list zipper, right_list zipper). *)
  val to_two_lists : 'a t -> 'a list * 'a list

  (** [left_length zipper] gets the length of [zipper]'s left list. *)
  val left_length : 'a t -> int

  (** [right_length zipper] gets the length of [zipper]'s right
     list. *)
  val right_length : 'a t -> int

  (** {3 Predicates} *)

  (** [is_at_start zipper] tests whether [zipper]'s left list is
      empty. *)
  val is_at_start : 'a t -> bool

  (** [is_at_end zipper] tests whether [zipper]'s right list is
      empty. *)
  val is_at_end : 'a t -> bool

  (** {3 Pushing} *)

  (** [push zipper ~value] pushes [value] into [zipper] at the cursor.
     The current cursor becomes the second item in the right list, and
     so on. *)
  val push : 'a t -> value:'a -> 'a t

  (** [push_left zipper ~value] pushes [value] into [zipper] just to
     the left of the cursor. *)
  val push_left : 'a t -> value:'a -> 'a t

  (** {3 Peeking and popping} *)

  (** [peek_opt ?steps zipper] retrieves the cursor value without
     popping it from the zipper.  If the cursor is empty, [None] is
     returned.

    If [steps] is given, it shifts the effective cursor [steps] places
     forwards. *)
  val peek_opt : ?steps:int -> 'a t -> 'a option

  (** [pop zipper] returns an error if [zipper] has no cursor, or [Ok
     (a, zipper')] where [a] is [zipper]'s cursor and [zipper'] is the
     new zipper formed by removing [a]. *)
  val pop : 'a t -> ('a * 'a t) Or_error.t

  (** [pop_opt zipper] behaves as {{!pop}pop}, but returns [None] if
     [zipper] has no cursor and [Some (a, zipper')] otherwise. *)
  val pop_opt : 'a t -> ('a * 'a t) option

  (** [map_head zipper ~f] maps [f] across the cursor of [zipper], if
     it exists, and replaces the cursor with the result (or drops it
     if [f] returns [None]). *)
  val map_head : 'a t -> f:('a -> 'a option) -> 'a t

  (** {3 Movement} *)

  (** [step ?steps zipper ~on_empty] takes one or more steps across
      [zipper].  The number of steps defaults to 1 (forwards), but
      can be given by [steps]; negative numbers step backwards through
      the zipper.  If the number of steps exceeds the bounds of the
      zipper, an error is returned. *)
  val step : ?steps:int -> 'a t -> 'a t Or_error.t
end

(** [S_monadic] contains the core operations of a zipper, parametrised
   over a particular failure monad. *)
module type S_monadic = sig
  type 'a t

  module M : Monad.S

  (** [pop_m zipper ~on_empty] behaves like {{!pop}pop}, but executes
     a custom monadic action [on_empty], instead of returning an
     error, when the cursor is empty. *)
  val pop_m : 'a t -> on_empty:('a t -> ('a * 'a t) M.t) -> ('a * 'a t) M.t

  (** [peek_m ?steps zipper ~on_empty] behaves like
     {{!peek_opt}peek_opt}, but executes a custom monadic action
     [on_empty], instead of returning [None], when the cursor is
     empty. *)
  val peek_m : ?steps:int -> 'a t -> on_empty:('a t -> 'a M.t) -> 'a M.t

  (** [step_m ?steps zipper ~on_empty] behaves like {{!step}step},
       but executes a custom monadic action [on_empty], instead of
       returning an error, when the cursor is empty. *)
  val step_m : ?steps:int -> 'a t -> on_empty:('a t -> 'a t M.t) -> 'a t M.t

  (** [map_m_head ?steps zipper ~on_empty] behaves like
       {{!map_head}map_head}, but executes a custom monadic action
       [on_empty], instead of leaving the zipper unchanged, when the
       cursor is empty. *)
  val map_m_head
    :  'a t
    -> f:('a -> 'a option M.t)
    -> on_empty:('a t -> 'a t M.t)
    -> 'a t M.t
end

(** [S] contains [S_non_monadic]; a functor for generating [S_monadic]
   over a custom monad; and specialisations of it over common
   monads. *)
module type S = sig
  include S_non_monadic

  (** [On_monad] provides various zipper operations parametrised by a
     monad. *)
  module On_monad (M : Monad.S) : S_monadic with type 'a t := 'a t and module M := M

  (** [On_ident] is [On_monad] specialised to the identity monad. *)
  module On_ident : module type of On_monad (Monad.Ident)

  (** [On_error] is [On_monad] specialised to the error monad. *)
  module On_error : module type of On_monad (Or_error)

  (** [On_option] is [On_monad] specialised to the option monad. *)
  module On_option : module type of On_monad (Option)
end

(** The type of instructions returned by functions used with
      [fold_until_m] and [fold_until]. *)
type ('mark, 'a, 'acc, 'final) fold_outcome =
  [ `Stop of 'final (** Stop folding, immediately return *)
  | `Drop of 'acc (** Drop the cursor and continue *)
  | `Swap of 'a * 'acc (** Replace cursor with a new value *)
  | `Mark of 'mark * 'a * 'acc (** Replace, and mark, the cursor *)
  ]

(** [S_marked_non_monadic] extends [S_non_monadic] to add functions
   for manipulating marks. *)
module type S_marked_non_monadic = sig
  include S_non_monadic

  (** The type of marks. *)
  type mark

  (** [mark zipper ~mark] marks the cursor with [mark], and returns
     the marked-up zipper.

      If the cursor is empty, an error is returned. *)
  val mark : 'a t -> mark:mark -> 'a t Or_error.t

  (** [recall zipper ~mark] rewinds [zipper] until the cursor is on an
     element previously marked with [mark].

      If [recall] runs out of left-list to rewind before finding
     [mark], an error is returned. *)
  val recall : 'a t -> mark:mark -> 'a t Or_error.t

  (** [fold_until zipper ~f ~init ~finish] behaves conceptually like
     {{!List.fold_until}List.fold_until}, but folds [f] through the
     remaining elements of a zipper.

    [f] receives the current accumulator, current cursor, and zipper
     with cursor popped at each stage.  It can't directly modify the
     zipper mid-fold, but can influence the value of the final zipper
     provided to the [finish] continuation by using the various legs
     of {{!fold_outcome}fold_outcome}. *)
  val fold_until
    :  'a t
    -> f:('acc -> 'a -> 'a t -> (mark, 'a, 'acc, 'final) fold_outcome)
    -> init:'acc
    -> finish:('acc -> 'a t -> 'final)
    -> 'final

  (** [delete_to_mark zipper ~mark] deletes every item in the
     left-list up to, and including, the element previously marked
     with [mark].

      If [delete_to_mark] runs out of left-list to rewind before
     finding [mark], an error is returned. *)
  val delete_to_mark : 'a t -> mark:mark -> 'a t Or_error.t
end

(** [S_marked_monadic] extends [S_monadic] to add functions for
   manipulating marks. *)
module type S_marked_monadic = sig
  include S_monadic

  (** The type of marks. *)
  type mark

  (** [mark_m zipper ~mark ~on_empty] behaves like {{!mark}mark},
        but executes a custom monadic action [on_empty], instead of
        returning an error, when the cursor is empty. *)
  val mark_m : 'a t -> mark:mark -> on_empty:('a t -> 'a t M.t) -> 'a t M.t

  (** [recall_m zipper ~mark ~on_empty] behaves like
     {{!recall}recall}, but executes a custom monadic action
     [on_empty], instead of returning an error, when the mark can't be
     found. *)
  val recall_m : 'a t -> mark:mark -> on_empty:('a t -> 'a t M.t) -> 'a t M.t

  (** [delete_to_mark_m zipper ~mark ~on_empty] behaves like
     {{!delete_to_mark}delete_to_mark}, but executes a custom monadic
     action [on_empty], instead of returning an error, when the mark
     can't be found. *)
  val delete_to_mark_m : 'a t -> mark:mark -> on_empty:('a t -> 'a t M.t) -> 'a t M.t

  (** [fold_m_until zipper ~f ~init ~finish] behaves like
       {{!fold_until}fold_until}, except that [f] and [finish], and
       therefore the function itself, return results inside a
       monad context. *)
  val fold_m_until
    :  'a t
    -> f:('acc -> 'a -> 'a t -> (mark, 'a, 'acc, 'final) fold_outcome M.t)
    -> init:'acc
    -> finish:('acc -> 'a t -> 'final M.t)
    -> 'final M.t
end

(** [S_marked] extends [S] to add functions for manipulating marks. *)
module type S_marked = sig
  include S_marked_non_monadic

  (** [On_monad] provides various marked zipper operations
     parametrised by a monad. *)
  module On_monad (M : Monad.S) :
    S_marked_monadic with type 'a t := 'a t and type mark := mark and module M := M

  (** [On_ident] is [On_monad] specialised to the identity monad. *)
  module On_ident : module type of On_monad (Monad.Ident)

  (** [On_error] is [On_monad] specialised to the error monad. *)
  module On_error : module type of On_monad (Or_error)

  (** [On_option] is [On_monad] specialised to the option monad. *)
  module On_option : module type of On_monad (Option)
end

(** [Basic_mark] is the interface that mark types must implement. *)
module type Basic_mark = sig
  type t [@@deriving sexp]

  include Comparable.S with type t := t
end
