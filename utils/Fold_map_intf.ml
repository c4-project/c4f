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

(** [Generic_monadic] is a signature describing a monadic fold-map on
   either an arity-0 or arity-1 container.

    This is analogous to [Core.Container.Generic] in its relationship
   with [S0] and [S1]. *)
module type Generic_monadic = sig
  (** [t] is the type of container to map over. *)
  type 'a t
  (** [elt] is the type of element inside the container. *)
  type 'a elt
  (** [M] is the monad over which we're fold-mapping. *)
  module M : Monad.S

  (** [fold_map ~f ~init c] folds [f] monadically over every [t] in
     [c], threading through an accumulator with initial value
     [init]. *)
  val fold_map
    :  f    : ('acc -> 'a elt -> ('acc * 'b elt) M.t)
    -> init : 'acc
    -> 'a t
    -> ('acc * 'b t) M.t
end

(** [Generic] is the non-monadic version of
    [Generic_monadic], generated using the identity monad. *)
module type Generic = Generic_monadic with module M := Monad.Ident

(** [S0_monadic] is the signature of a monadic fold-map over
    arity-0 containers. *)
module type S0_monadic = sig
  type t
  type elt

  (** [S0]s can fold-map: the container type is always [t],
      and the element type is always [elt]. *)
  include Generic_monadic with type 'a t := t and type 'a elt := elt
end

(** [S1_monadic] is the signature of a monadic fold-map over
    arity-1 containers. *)
module type S1_monadic = sig
  (** [t] is the type of the container to map over. *)
  type 'a t

  (** [S0]s can fold-map: when the container type is ['a t],
      the element type is ['a]. *)
  include Generic_monadic with type 'a t := 'a t and type 'a elt := 'a
end

(** We can derive the non-monadic fold-map signatures by
    applying the monadic ones to the identity monad. *)

(** [S0] is the signature of a basic fold-map over
    arity-0 containers. *)
module type S0 = S0_monadic with module M := Monad.Ident

(** [S1] is the signature of a basic fold-map over
    arity-1 containers. *)
module type S1 = S1_monadic with module M := Monad.Ident

(** Building containers from fold-mappable types *)

(** [Basic_container0] is the signature that fold-mappable containers of
    arity 0 must implement. *)
module type Basic_container0 = sig
  (** [t] is the container type. *)
  type t

  (** [Elt] contains the element type, which must have equality. *)
  module Elt : Equal.S

  (** [On_monad] implements the monadic fold-map for a given monad [M]. *)
  module On_monad
    : functor (MS : Monad.S)
      -> S0_monadic with type t := t
                     and type elt := Elt.t
                     and module M := MS
  ;;
end

(** [Basic_container1] is the signature that fold-mappable containers
   of arity 1 must implement. *)
module type Basic_container1 = sig
  (** [t] is the container type. *)
  type 'a t

  (** [On_monad] implements the monadic fold-map for a given monad [M]. *)
  module On_monad
    : functor (MS : Monad.S)
      -> S1_monadic with type 'a t := 'a t
                     and module M := MS
  ;;
end

(** [Generic_on_monad] extends [Generic] to contain various derived
   operators; we use it to derive the signatures of the various
   [On_monad] modules. *)
module type Generic_on_monad = sig
  include Generic_monadic

  (** [foldM x ~init ~f] folds the monadic computation [f] over [x],
      starting with initial value [init], and returning the final
      value inside the monadic effect. *)
  val foldM
    :  'a t
    -> init : 'acc
    -> f    : ('acc -> 'a elt -> 'acc M.t)
    -> 'acc M.t
  ;;

  (** [iterM x ~f] iterates the monadic computation [f] over [x],
      returning the final monadic effect. *)
  val iterM
    :  'a t
    -> f : ('a elt -> unit M.t)
    -> unit M.t
  ;;

  (** [mapM ~f x] maps the monadic computation [f] over [x],
      collecting the monadic effect and returning the result
      inside it. *)
  val mapM : f : ('a elt -> 'b elt M.t) -> 'a t -> 'b t M.t

  (** [mapiM ~f x] behaves as [mapM], but also supplies [f] with the
      index of the element.  This index should match the actual
      position of the element in the container [x]. *)
  val mapiM : f : (int -> 'a elt -> 'b elt M.t) -> 'a t -> 'b t M.t
end

(** [Generic_container] is a generic interface for fold-mappable
   containers, used to build [Container0] (arity-0) and [Container1]
   (arity-1). *)
module type Generic_container = sig
  type 'a t
  type 'a elt

  (** [On_monad] implements monadic folding and mapping operators for
      a given monad [M]. *)
  module On_monad
    : functor (MS : Monad.S)
      -> Generic_on_monad with type 'a t   := 'a t
                           and type 'a elt := 'a elt
                           and module M := MS
  ;;

  (** We can do generic container operations. *)
  include Container.Generic with type 'a t   := 'a t
                             and type 'a elt := 'a elt
  (** We can do non-monadic fold-map operations. *)
  include Generic           with type 'a t   := 'a t
                             and type 'a elt := 'a elt

  (** [map ~f t] maps [f] across [t] without accumulating anything. *)
  val map : f : ('a elt -> 'b elt) -> 'a t -> 'b t

  (** [mapi ~f t] maps [f] across [t], passing in an increasing
      position counter. *)
  val mapi : f : (int -> 'a elt -> 'b elt) -> 'a t -> 'b t

  (** [With_errors] specialises [On_monad] to the error monad. *)
  module With_errors : Generic_on_monad with type 'a t := 'a t
                                         and type 'a elt := 'a elt
                                         and module M := Or_error
  ;;
end

(** [Container0] is a generic interface for arity-0 fold-mappable
    containers. *)
module type Container0 = sig
  (** [t] is the type of the container. *)
  type t
  (** [elt] is the type of elements. *)
  type elt

  include Generic_container with type 'a t := t and type 'a elt := elt
  include Container.S0 with type t := t and type elt := elt
end

(** [Container1] is a generic interface for arity-1 fold-mappable
    containers.  It also incldues [My_container.Extensions1]. *)
module type Container1 = sig
  (** ['a t] is the type of the container, parametrised over the
      element type ['a]. *)
  type 'a t

  include Generic_container with type 'a t := 'a t and type 'a elt := 'a
  include Container.S1 with type 'a t := 'a t
  include My_container.Extensions1 with type 'a t := 'a t

  (** [right_pad ~padding xs] pads every list in xs with [padding],
      ensuring all lists have equal length. *)
  val right_pad : padding:'a -> 'a list t -> 'a list t

  (** [With_elt (Elt)] demotes this [Container1] to a [Container0] by
     fixing the element type to that mentioned in [Elt]. *)
  module With_elt
    : functor (Elt : Equal.S)
      -> Container0 with type t := Elt.t t and type elt := Elt.t
  ;;
end

(** [Fold_map] contains things to export in [Fold_map.mli]. *)
module type Fold_map = sig
  module type Generic = Generic
  module type S0_monadic = S0_monadic
  module type S1_monadic = S1_monadic
  module type S0 = S0
  module type S1 = S1
  module type Generic_on_monad = Generic_on_monad
  module type Generic_container = Generic_container
  module type Basic_container0 = Basic_container0
  module type Basic_container1 = Basic_container1
  module type Container0 = Container0
  module type Container1 = Container1

  (** [Make_container0] takes a [Basic_container0] and implements all
     of the derived functions in [Container0]. *)
  module Make_container0
    : functor (I : Basic_container0)
      -> Container0 with type t := I.t and type elt := I.Elt.t
  ;;

  (** [Make_container1] takes a [Basic_container1] and implements all
     of the derived functions in [Container1]. *)
  module Make_container1
    : functor (I : Basic_container1)
      -> Container1 with type 'a t := 'a I.t
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

  (** [Option] provides monadic fold-mapping for optional
      values. *)
  module Option : Container1 with type 'a t := 'a option

  (** [chain mapper ~f] lifts a fold-map of [f] on an inner container
     to a fold-mapping function on an outer container. *)
  val chain
    :  (f:'f -> init:'i -> 'result)
    -> f : 'f
    -> 'i
    -> 'result
  ;;
end
