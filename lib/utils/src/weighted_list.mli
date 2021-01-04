(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Weighted lists

    Weighted lists are lists in which each item has an attached non-negative
    integer weight. This weight is then used when selecting random items from
    the list: an item of weight [2*n] is twice as likely to be selected over
    an item of weight [n].

    This is similar in spirit to Core_extended's [Sampler], but slightly
    different in execution. *)

open Base

(** Opaque type of weighted lists, parametrised on value. *)
type 'a t [@@deriving sexp_of, quickcheck]

(** {1 Constructors} *)

val from_alist : ('a, int) List.Assoc.t -> 'a t Or_error.t
(** [from_alist alist] tries to convert the associative list [alist], which
    maps values to their weights, to a weighted list. It fails if [alist] is
    empty, or if any weight is negative (note that weights may be zero). *)

(** {1 Operations} *)

val adjust_weights : 'a t -> f:('a -> int -> int) -> 'a t Or_error.t
(** [adjust_weights wl ~f] maps [f] over every weight in [table], providing
    the weighted item for context. It fails if any invocation of [f] tries to
    return a negative weight (note that weights may be zero). *)

val find : 'a t -> f:('a -> bool) -> 'a option
(** [find wl ~f] finds the first item in [wl] for which [f] is true, if any. *)

val fold : 'a t -> f:('b -> 'a -> int -> 'b) -> init:'b -> 'b
(** [fold wl ~f ~init] folds [f] over every row in [wl], starting at [init]. *)

val iter : 'a t -> f:('a -> int -> unit) -> unit
(** [iter wl ~f ~init] applies side-effectful function [f] to every row in
    [wl]. *)

(** {1 Sampling} *)

val sample : 'a t -> random:Splittable_random.State.t -> 'a option
(** [sample wl ~random] converts [wl] to a cumulative list, then samples it
    according to the random number generator [random]. It fails if the
    conversion fails.

    If you sample from the same list often, consider converting it to a
    cumulative list once, then sampling that directly. *)

val sample_gen_exn : 'a t -> 'a Base_quickcheck.Generator.t
(** [sample_gen_exn wl] is [sample], but wrapped up as a Quickcheck generator
    that raises exceptions on failure. *)

(** Type of weighted lists that have been re-arranged to list weights
    cumulatively. *)
module Cumulative : sig
  (** Synonym for weighted lists. *)
  type 'a w = 'a t

  (** Opaque type of cumulative lists. *)
  type 'a t [@@deriving sexp_of]

  val of_weighted_list : 'a w -> 'a t option
  (** [of_weighted_list wl] converts [wl] to a cumulative list. It fails if
      there are no items in [wl] with a nonzero weight. *)

  val sample : 'a t -> random:Splittable_random.State.t -> 'a
  (** [sample cl ~random] samples the cumulative list [cl] according to the
      random number generator [random]. *)
end
