(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Utility functions for lists. *)

open Base

(** {1 Spans} *)

module Span : sig
  (** A span, containing a position and length. *)
  type t = {pos: int; len: int} [@@deriving sexp]

  val end_pos : t -> int
  (** [end_pos] is the end position of the span: [pos + len]. *)
end

(** {1 Finding and transforming specific amounts of items} *)

val find_at_most_one :
     ?item_name:string
  -> 'a list
  -> f:('a -> 'b option)
  -> on_empty:'b Or_error.t
  -> 'b Or_error.t
(** [find_at_most_one ?item_name items ~f ~on_empty] tries to find an item in
    [items] for which [f] returns [Some]. It returns the result if precisely
    one exists; [on_empty] if none exist, and an error otherwise. *)

val find_one_opt :
  ?item_name:string -> 'a list -> f:('a -> 'b option) -> 'b option Or_error.t
(** [find_one_opt ?item_name items ~f] tries to find an item in [items] for
    which [f] returns [Some]. It returns the result in [Some] if precisely
    one exists, [None] if none exist, and an error otherwise. *)

val find_one :
  ?item_name:string -> 'a list -> f:('a -> 'b option) -> 'b Or_error.t
(** [find_one ?item_name items ~f] tries to find an item in [items] for which
    [f] returns [Some]. It returns the result if precisely one exists, and an
    error otherwise. *)

(** {1 Using a splittable RNG to access elements} *)

module Random : sig
  val index : _ list -> random:Splittable_random.State.t -> int option
  (** [index xs ~random] returns [None] if [xs] is empty, and [Some i], where
      [i] is a random number generated with [random] between [0] and
      [length xs] exclusive, otherwise. *)

  val span : _ list -> random:Splittable_random.State.t -> Span.t option
  (** [index xs ~random] returns [None] if [xs] is empty, otherwise
      [Some {pos; len}], where [pos] is a random index generated with
      [random] between [0] and [length xs] exclusive and [len] is a random
      length generated with [random] between [0] and the [length] of the
      sub-list after [pos]. *)

  val item : 'a list -> random:Splittable_random.State.t -> 'a option
  (** [item xs ~random] behaves like Base's [List.random_element], but uses a
      splittable RNG for compatibility with Quickcheck etc. *)
end

(** {1 Manipulating lists} *)

val split_or_error : 'a list -> int -> ('a list * 'a list) Or_error.t
(** [split_or_error xs n] behaves like [split xs n], but returns an error if
    [n] is negative or greater than the length of the list. *)

val splice :
     'a list
  -> span:Span.t
  -> replace_f:('a list -> 'a list)
  -> 'a list Or_error.t
(** [splice xs ~span ~replace_f] replaces the part of [xs] denoted by [pos]
    and [len] with its image in [replace_f]. It returns an error if [pos] is
    out of bounds, or the list from [pos] onwards is shorter than [len]. *)

val try_splice :
     'a list
  -> span:Span.t
  -> replace_f:('a list -> 'a list Or_error.t)
  -> 'a list Or_error.t
(** [try_splice xs ~span ~replace_f] behaves as {!splice}, but accepts a
    [replace_f] that can fail. *)

val map_sub : 'a list -> span:Span.t -> f:('a -> 'a) -> 'a list Or_error.t
(** [map_sub xs ~span ~f] maps [f] over the part of [xs] denoted by [pos] and
    [len]. *)

val try_map_sub :
  'a list -> span:Span.t -> f:('a -> 'a Or_error.t) -> 'a list Or_error.t
(** [try_map_sub xs ~span ~f] maps [f] over the part of [xs] denoted by [pos]
    and [len]. *)

val eval_guards : (bool * (unit -> 'a)) list -> 'a list
(** [eval_guards xs] filters [xs] to those entries [(b, f)] for which [b] is
    true, and then maps to [f ()] for each. It is useful for presenting to
    quickcheck [union] generators a list of sub-generators that may or may
    not be viable at run-time depending on a Boolean flag. *)

(** {2 Guarding a function based on the emptiness of a list}

    These functions don't distinguish in the type system between a non-empty
    list and a normal list; this behaviour may change eventually. *)

val guard_if_empty_opt : 'a list -> f:('a list -> 'b option) -> 'b option
(** [guard_if_empty_opt xs f] evaluates to [f xs] if [xs] is non-empty, and
    [None] otherwise. *)

val guard_if_empty : 'a list -> f:('a list -> 'b) -> 'b option
(** [guard_if_empty xs f] evaluates to [Some (f xs)] if [xs] is non-empty,
    and [None] otherwise. *)
