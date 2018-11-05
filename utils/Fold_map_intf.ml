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

open Core

(*
 * Signatures only containing the fold-map operations
 *)

(** [C0] is a signature containing the base and element types of an
    arity-0 container. *)
module type C0 = sig
  (** [t] is the type of container to map over. *)
  type t
  (** [elt] is the type of element inside the container. *)
  type elt
end

(** [Mappable0] is the signature of a plain fold-map over
    arity-0 containers. *)
module type Mappable0 = sig
  include C0

  (** [fold_map ~f ~init c] folds [f] over every [t] in [c],
      threading through an accumulator with initial value [init].
      Order is not guaranteed. *)
  val fold_map
    :  f    : ('a -> elt -> ('a * elt))
    -> init : 'a
    -> t
    -> ('a * t)
end

(** [Mappable1] is the signature of a plain fold-map over
    arity-1 containers. *)
module type Mappable1 = sig
  (** [t] is the type of the container to map over. *)
  type 'a t

  (** [fold_map ~f ~init c] folds [f] over every [t] in [c],
      threading through an accumulator with initial value [init].
      Order is not guaranteed. *)
  val fold_map
    :  f    : ('s -> 'a -> ('s * 'b))
    -> init : 's
    -> 'a t
    -> ('s * 'b t)
end

(** [Mappable0_monadic] is the signature of a monadic fold-map over
    arity-0 containers. *)
module type Mappable0_monadic = sig
  include C0

  (** [M] is the monad to map over. *)
  module M : Monad.S

  (** [fold_map ~f ~init c] folds [f] over every [t] in [c],
      threading through an accumulator with initial value [init], and
      also threading through a monad of type [M.t].
      Order is not guaranteed. *)
  val fold_map
    :  f    : ('a -> elt -> ('a * elt) M.t)
    -> init : 'a
    -> t
    -> ('a * t) M.t
end


(** [Mappable1_monadic] is the signature of a monadic fold-map over
    arity-1 containers. *)
module type Mappable1_monadic = sig
  (** [t] is the type of the container to map over. *)
  type 'a t

  (** [M] is the monad to map over. *)
  module M : Monad.S

  (** [fold_map ~f ~init c] folds [f] over every [t] in [c],
      threading through an accumulator with initial value [init], and
      also threading through a monad of type [M.t].
      Order is not guaranteed. *)
  val fold_map
    :  f    : ('s -> 'a -> ('s * 'b) M.t)
    -> init : 's
    -> 'a t
    -> ('s * 'b t) M.t
end

(*
 * Building containers from fold-mappable types
 *)

(** [Basic0] is the signature that fold-mappable containers of
    arity 0 must implement. *)
module type Basic0 = sig
  (** [t] is the container type. *)
  type t

  (** [Elt] contains the element type, which must have equality. *)
  module Elt : Equal.S

  (** [On_monad] implements the monadic fold-map for a given monad [M]. *)
  module On_monad
    : functor (MS : Monad.S)
      -> Mappable0_monadic with type t := t
                            and type elt := Elt.t
                            and module M := MS
  ;;
end

(** [Basic1] is the signature that fold-mappable containers of
    arity 1 must implement. *)
module type Basic1 = sig
  (** [t] is the container type. *)
  type 'a t

  (** [On_monad] implements the monadic fold-map for a given monad [M]. *)
  module On_monad
    : functor (MS : Monad.S)
      -> Mappable1_monadic with type 'a t := 'a t
                            and module M := MS
  ;;
end

(** [S0_monadic] extends [Mappable0_monadic] to contain various derived
    operators. *)
module type S0_monadic = sig
  include Mappable0_monadic

  (** [foldM x ~init ~f] folds the monadic computation [f] over [x],
      starting with initial value [init], and returning the final
      value inside the monadic effect. *)
  val foldM
    :  t
    -> init : 'acc
    -> f    : ('acc -> elt -> 'acc M.t)
    -> 'acc M.t
  ;;

  (** [mapM ~f x] maps the monadic computation [f] over [x],
      collecting the monadic effect and returning the result
      inside it. *)
  val mapM : f : (elt -> elt M.t) -> t -> t M.t

  (** [mapiM ~f x] behaves as [mapM], but also supplies [f] with the
      index of the element.  This index should match the actual
      position of the element in the container [x]. *)
  val mapiM : f : (int -> elt -> elt M.t) -> t -> t M.t
end

(** [S1_monadic] extends [Mappable1_monadic] to contain various derived
    operators. *)
module type S1_monadic = sig
  include Mappable1_monadic

  (** [foldM x ~init ~f] folds the monadic computation [f] over [x],
      starting with initial value [init], and returning the final
      value inside the monadic effect. *)
  val foldM
    :  'elt t
    -> init : 'acc
    -> f    : ('acc -> 'elt -> 'acc M.t)
    -> 'acc M.t
  ;;

  (** [mapM ~f x] maps the monadic computation [f] over [x],
      collecting the monadic effect and returning the result
      inside it. *)
  val mapM : f : ('a -> 'b M.t) -> 'a t -> 'b t M.t

  (** [mapiM ~f x] behaves as [mapM], but also supplies [f] with the
      index of the element.  This index should match the actual
      position of the element in the container [x]. *)
  val mapiM : f : (int -> 'a -> 'b M.t) -> 'a t -> 'b t M.t
end

(** [S0] is the interface of 'full' [Fold_map] implementations, eg
    those generated by [Make], over arity-0 containers. *)
module type S0 = sig
  include C0

  (** [On_monad] implements monadic folding and mapping operators for
      a given monad [M]. *)
  module On_monad
    : functor (MS : Monad.S)
      -> S0_monadic with type t := t
                     and type elt := elt
                     and module M := MS
  ;;

  include Container.S0 with type t := t and type elt := elt
  include Mappable0    with type t := t and type elt := elt

  (** [map ~f t] maps [f] across [t] without accumulating anything. *)
  val map : f : (elt -> elt) -> t -> t

  (** [max_measure ~measure ?default xs] measures each item in [xs]
      according to [measure], and returns the highest measure reported.
      If [xs] is empty, return [default] if given, and [0]
      otherwise. *)
  val max_measure : measure:(elt -> int) -> ?default:int -> t -> int

  (** [With_errors] specialises [On_monad] to the error monad. *)
  module With_errors : S0_monadic with type t := t
                                   and type elt := elt
                                   and module M := Or_error
  ;;
end

(** [S1] is the interface of 'full' [Fold_map] implementations, eg
    those generated by [Make], over arity-1 containers. *)
module type S1 = sig
  type 'a t

  (** [On_monad] implements monadic folding and mapping operators for
      a given monad [M]. *)
  module On_monad
    : functor (MS : Monad.S)
      -> S1_monadic with type 'a t := 'a t
                     and module M := MS
  ;;

  include Container.S1 with type 'a t := 'a t
  include Mappable1    with type 'a t := 'a t

  (** [map ~f t] maps [f] across [t] without accumulating anything. *)
  val map : f : ('a -> 'b) -> 'a t -> 'b t

  (** [max_measure ~measure ?default xs] measures each item in [xs]
      according to [measure], and returns the highest measure reported.
      If [xs] is empty, return [default] if given, and [0]
      otherwise. *)
  val max_measure : measure:('a -> int) -> ?default:int -> 'a t -> int

  (** [right_pad ~padding xs] pads every list in xs with [padding],
      ensuring all lists have equal length. *)
  val right_pad : padding:'a -> 'a list t -> 'a list t

  (** [With_errors] specialises [On_monad] to the error monad. *)
  module With_errors : S1_monadic with type 'a t := 'a t
                                   and module M := Or_error
  ;;

  (** [To_S0 (Elt)] demotes this [S1] to an [S0] by fixing the element
      type to that mentioned in [Elt]. *)
  module To_S0
    : functor (Elt : Equal.S)
      -> S0 with type t := Elt.t t and type elt := Elt.t
  ;;
end

(** [Fold_map] contains things to export in [Fold_map.mli]. *)
module type Fold_map = sig
  module type Mappable0 = Mappable0
  module type Mappable1 = Mappable1
  module type Mappable0_monadic = Mappable0_monadic
  module type Mappable1_monadic = Mappable1_monadic
  module type Basic0 = Basic0
  module type Basic1 = Basic1
  module type S0_monadic = S0_monadic
  module type S1_monadic = S1_monadic
  module type S0 = S0
  module type S1 = S1

  (** [Make0] takes a [Basic0] and implements all of the derived functions
        in [S0]. *)
  module Make0
    : functor (I : Basic0)
      -> S0 with type t = I.t and type elt = I.Elt.t
  ;;

  (** [Make1] takes a [Basic1] and implements all of the derived functions
        in [S1]. *)
  module Make1
    : functor (I : Basic1)
      -> S1 with type 'a t = 'a I.t
  ;;

  (** [Helpers] contains utility functions for building monadic
     [Fold_map]s.

      Functions beginning [proc_variant] are useful for building
     fold-map functions on top of Variantslib's [map] function.  The
     function [proc_field] is useful for building fold-map functions
     on top of Fieldslib's [fold] function. *)
  module Helpers : functor (M : Monad.S) -> sig
    (** [proc_variant0 f init variant] lifts a fold-map operation into
       a fold-map operation over a nullary variant constructor
       [variant], using the Variantslib folder. *)
    val proc_variant0
      :  ('acc -> unit -> ('acc * unit) M.t)
      -> 'acc
      -> 'a Base.Variant.t
      -> ('acc * 'a) M.t
    ;;

    (** [proc_variant1 f init variant a] lifts a fold-map operation
       [f] with initial accumulator [init] over a Variantslib unary
       variant constructor [variant] with argument [a]. *)
    val proc_variant1
      :  ('acc -> 'a -> ('acc * 'a) M.t)
      -> 'acc
      -> ('a -> 'b) Base.Variant.t
      -> 'a
      -> ('acc * 'b) M.t
    ;;

    (** [proc_variant2 f init variant a b] lifts a fold-map operation
       [f] with initial accumulator [init] over a Variantslib binary
       variant constructor [variant] with arguments [a] and [b]. *)
    val proc_variant2
      :  ('acc -> ('a * 'b) -> ('acc * ('a * 'b)) M.t)
      -> 'acc
      -> ('a -> 'b -> 'c) Base.Variant.t
      -> 'a
      -> 'b
      -> ('acc * 'c) M.t
    ;;

    (** [proc_variant3 f init variant a b c] lifts a fold-map
       operation [f] with initial accumulator [init] over a
       Variantslib ternary variant constructor [variant] with
       arguments [a], [b], and [c]. *)
    val proc_variant3
      :  ('acc -> ('a * 'b * 'c) -> ('acc * ('a * 'b * 'c)) M.t)
      -> 'acc
      -> ('a -> 'b -> 'c -> 'd) Base.Variant.t
      -> 'a
      -> 'b
      -> 'c
      -> ('acc * 'd) M.t
    ;;

    (** [proc_field f state field container original] lifts a fold-map
       operation [f] to a form comparible with Fieldslib's [fold]
       function. *)
    val proc_field
      :  ('acc -> 'elt -> ('acc * 'elt) M.t)
      -> ('acc * 't) M.t
      -> ([> `Set_and_create], 't, 'elt) Field.t_with_perm
      -> 't (** Unused, but needed for Fieldslib compatibility *)
      -> 'elt
      -> ('acc * 't) M.t
    ;;

    (** [fold_nop state item] is a function that has the right signature
        to use in monadic fold-maps, but does nothing. *)
    val fold_nop : 'b -> 'a -> ('b * 'a) M.t
  end

  (** Implementations for common containers

      Many of these are re-exported in [MyContainers] alongside other
      container extensions. *)

  (** [List] provides monadic fold-mapping for a list of
      elements. *)
  module List : S1 with type 'a t = 'a list

  (** [Option] provides monadic fold-mapping for optional
      values. *)
  module Option : S1 with type 'a t = 'a option

  (** [Singleton] provides monadic fold-mapping over a single
      values.  It's useful for passing a single value to something
      that expects a fold-mappable container. *)
  module Singleton : S1 with type 'a t = 'a

  (** [chain mapper ~f] lifts a fold-map of [f] on an inner container
     to a fold-mapping function on an outer container. *)
  val chain
    :  (f:'f -> init:'i -> 'result)
    -> f : 'f
    -> 'i
    -> 'result
  ;;
end
