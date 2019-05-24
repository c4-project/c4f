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

(** Weighted lists

    Weighted lists are lists in which each item has an attached non-negative
    integer weight. This weight is then used when selecting random items
    from the list: an item of weight [2*n] is twice as likely to be selected
    over an item of weight [n].

    This is similar in spirit to Core_extended's [Sampler], but slightly
    different in execution. *)

open Core_kernel

(** Opaque type of weighted lists, parametrised on value. *)
type 'a t [@@deriving sexp_of, quickcheck]

(** {2 Constructors} *)

val from_alist : ('a, int) List.Assoc.t -> 'a t Or_error.t
(** [from_alist alist] tries to convert the associative list [alist], which
    maps values to their weights, to a weighted list. It fails if [alist] is
    empty, or if any weight is negative (note that weights may be zero). *)

val from_alist_exn : ('a, int) List.Assoc.t -> 'a t
(** [from_alist_exn alist] does the same thing as
    {{!from_alist} from_alist}, but raises an exception if the table is
    ill-formed. *)

(** {2 Modifications} *)

val adjust_weights : 'a t -> f:('a -> int -> int) -> 'a t Or_error.t
(** [adjust_weights wl ~f] maps [f] over every weight in [table], providing
    the weighted item for context. It fails if any invocation of [f] tries
    to return a negative weight (note that weights may be zero). *)

val adjust_weights_exn : 'a t -> f:('a -> int -> int) -> 'a t
(** [adjust_weights_exn wl ~f] does the same thing as
    {{!adjust_weights} adjust_weights}, but raises an exception if [f]
    returns a negative weight. *)

(** [On_monad] generalises [adjust_weights] to concern any monad [M] which
    is a monad transformer over errors. *)
module On_monad (M : sig
  (* TODO(@MattWindsor91): this should really be in Travesty *)
  include Monad.S

  val lift : 'a Or_error.t -> 'a t
end) : sig
  val adjust_weights_m : 'a t -> f:('a -> int -> int M.t) -> 'a t M.t
  (** [adjust_weights_m wl ~f] maps [f] over every weight in [table],
      providing the weighted item for context. It expects [f] to return
      values in the monad [M], and returns the final result in the same
      monad. *)
end

(** {2 Conversion} *)

val fold : 'a t -> f:('b -> 'a -> int -> 'b) -> init:'b -> 'b
(** [fold wl ~f ~init] folds [f] over every row in [wl], starting at [init]. *)

val iter : 'a t -> f:('a -> int -> unit) -> unit
(** [iter wl ~f ~init] applies side-effectful function [f] to every row in
    [wl]. *)

(** {2 Sampling} *)

(** Type of weighted lists that have been re-arranged to list weights
    cumulatively. *)
module Cumulative : sig
  (** Synonym for weighted lists. *)
  type 'a w = 'a t

  (** Opaque type of cumulative lists. *)
  type 'a t [@@deriving sexp_of]

  val of_weighted_list : 'a w -> 'a t Or_error.t
  (** [of_weighted_list wl] converts [wl] to a cumulative list. It fails if
      there are no items in [wl] with a nonzero weight. *)

  val sample : 'a t -> random:Splittable_random.State.t -> 'a
  (** [sample cl ~random] samples the cumulative list [cl] according to the
      random number generator [random]. *)
end

val sample : 'a t -> random:Splittable_random.State.t -> 'a Or_error.t
(** [sample wl ~random] converts [wl] to a cumulative list, then samples it
    according to the random number generator [random]. It fails if the
    conversion fails.

    If you sample from the same list often, consider converting it to a
    cumulative list once, then sampling that directly. *)

val sample_exn : 'a t -> random:Splittable_random.State.t -> 'a
(** [sample_exn wl ~random] is [sample wl ~random], but raises an exception
    rather than returning an error monad. *)

val sample_gen_exn : 'a t -> 'a Quickcheck.Generator.t
(** [sample_gen_exn wl] is [sample_exn], but wrapped up as a Quickcheck
    generator. *)
