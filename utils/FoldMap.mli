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

(** Lightweight modules for fold-mapping over first-order containers
   *)

(** [S] is the signature that fold-mappable containers must
   implement. *)
module type S = sig
  type t
  type cont

  (** [fold_map ~f ~init c] folds [f] over every [t] in [c],
      threading through an accumulator with initial value [init].
      Order is not guaranteed. *)
  val fold_map
    :  f:('a -> t -> 'a * t)
    -> init:'a
    -> cont
    -> ('a * cont)
end

(** [Intf] is the interface of 'full' [FoldMap] implementations, eg
    those generated by [Make]. *)
module type Intf = sig
  include S

  (** [map ~f c] maps [f] over every [t] in [c]. *)
  val map : f:(t -> t) -> cont -> cont

  (** [fold ~f c] folds [f] over every [t] in [c]. *)
  val fold : f:('a -> t -> 'a) -> init:'a -> cont -> 'a

  (** [list c] retrieves a list of all [t] in [c]. *)
  val list : cont -> t list

  (** [for_all ~f c] returns true if [f] holds for all [t] in [c]. *)
  val for_all : f:(t -> bool) -> cont -> bool

  (** [exists ~f c] returns true if [f] holds for at least one [t] in
     [c]. *)
  val exists : f:(t -> bool) -> cont -> bool
end

(** [Make] takes an [S] and implements all of the derived functions
      in [Intf]. *)
module Make
  : functor (I : S)
    -> Intf with type t = I.t
             and type cont = I.cont

(** [SetIntf] extends [Intf] to add set generation operations;
    these only work if [t] has a [Set] module available. *)
module type SetIntf = sig
  include Intf

  module Set : Core.Set.S with type Elt.t = t

  (** [set c] retrieves the set of all [t] in [c]. *)
  val set : cont -> Set.t
end

(** [MakeSet] takes an [S] and related [Set.S], and implements all of
   the derived functions in [SetIntf]. *)
module MakeSet
  : functor (I : S)
    -> functor (Set : Core.Set.S with type Elt.t = I.t)
      -> SetIntf with type t = I.t
                  and type cont = I.cont
                  and module Set = Set
