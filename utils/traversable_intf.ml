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

open Base

(** [Generic] is a signature describing a monadic traversal on
   either an arity-0 or arity-1 container.

    This is analogous to [Mappable.Generic] in its relationship
   with [S0] and [S1]. *)
module type Generic = sig
  include Mappable.Generic_types

  (** [M] is the monad over which we're fold-mapping. *)
  module M : Monad.S

  (** [map_m c ~f] maps [f] over every [t] in [c], threading through
     monadic state. *)
  val map_m : 'a t -> f:('a elt -> 'b elt M.t) -> 'b t M.t
end

(** [S0] is the signature of a monadic traversal over arity-0
   containers. *)
module type S0 = sig
  type t
  type elt

  include Generic with type 'a t := t and type 'a elt := elt
end

(** [S1] is the signature of a monadic traversal over arity-1
   containers. *)
module type S1 = sig
  (** [t] is the type of the container to map over. *)
  type 'a t

  (** [S1]s can traverse: when the container type is ['a t],
      the element type is ['a]. *)
  include Generic with type 'a t := 'a t and type 'a elt := 'a
end

(** Building containers from traversable types *)

(** [Basic_container0] is the signature that traversable containers of
   arity 0 must implement. *)
module type Basic_container0 = sig
  (** [t] is the container type. *)
  type t

  (** [Elt] contains the element type, which must have equality. *)
  module Elt : Equal.S

  (** [On_monad] implements the monadic traversal for a given monad
     [M]. *)
  module On_monad
    : functor (MS : Monad.S)
      -> S0 with type t := t
             and type elt := Elt.t
             and module M := MS
  ;;
end

(** [Basic_container1] is the signature that traversable containers
   of arity 1 must implement. *)
module type Basic_container1 = sig
  (** [t] is the container type. *)
  type 'a t

  (** [On_monad] implements the monadic traversable for a given monad
     [M]. *)
  module On_monad
    : functor (MS : Monad.S)
      -> S1 with type 'a t := 'a t
             and module M := MS
  ;;
end

(** [Generic_on_monad] extends [Generic] to contain various derived
   operators; we use it to derive the signatures of the various
   [On_monad] modules. *)
module type Generic_on_monad = sig
  include Generic

  (** [fold_map_m c ~f ~init] folds [f] monadically over every [t] in
     [c], threading through an accumulator with initial value
     [init]. *)
  val fold_map_m
    :  'a t
    -> f    : ('acc -> 'a elt -> ('acc * 'b elt) M.t)
    -> init : 'acc
    -> ('acc * 'b t) M.t
  ;;

  (** [fold_m x ~init ~f] folds the monadic computation [f] over [x],
      starting with initial value [init], and returning the final
      value inside the monadic effect. *)
  val fold_m
    :  'a t
    -> init : 'acc
    -> f    : ('acc -> 'a elt -> 'acc M.t)
    -> 'acc M.t
  ;;

  (** [iter_m x ~f] iterates the monadic computation [f] over [x],
      returning the final monadic effect. *)
  val iter_m
    :  'a t
    -> f : ('a elt -> unit M.t)
    -> unit M.t
  ;;

  (** [mapi_m ~f x] behaves as [mapM], but also supplies [f] with the
      index of the element.  This index should match the actual
      position of the element in the container [x]. *)
  val mapi_m : f : (int -> 'a elt -> 'b elt M.t) -> 'a t -> 'b t M.t
end

(** [On_monad1] extends [Generic_on_monad] with functionality that
    only works on arity-1 containers. *)
module type On_monad1 = sig
  type 'a t

  include Generic_on_monad with type 'a t := 'a t and type 'a elt := 'a

  (** [sequence_m x] lifts a container of monads [x] to a monad
      containing a container, by sequencing the monadic effects from
      left to right. *)
  val sequence_m : 'a M.t t -> 'a t M.t
end

(** [Generic_container] is a generic interface for traversable
   containers, used to build [Container0] (arity-0) and [Container1]
   (arity-1). *)
module type Generic_container = sig
  include Mappable.Generic_types

  (** [On_monad] implements monadic traversal operators for
      a given monad [M]. *)
  module On_monad
    : functor (M : Monad.S)
      -> Generic_on_monad with type 'a t   := 'a t
                           and type 'a elt := 'a elt
                           and module M := M
  ;;

  (** We can do generic container operations. *)
  include Container.Generic with type 'a t   := 'a t
                             and type 'a elt := 'a elt

  (** We can do non-monadic mapping operations. *)
  include Mappable.Generic with type 'a t   := 'a t
                            and type 'a elt := 'a elt

  (** [fold_map c ~f ~init] folds [f] over every [t] in [c], threading
     through an accumulator with initial value [init]. *)
  val fold_map
    :  'a t
    -> f    : ('acc -> 'a elt -> ('acc * 'b elt))
    -> init : 'acc
    -> ('acc * 'b t)
  ;;

  (** [mapi ~f t] maps [f] across [t], passing in an increasing
      position counter. *)
  val mapi : f : (int -> 'a elt -> 'b elt) -> 'a t -> 'b t

  (** [With_errors] specialises [On_monad] to the error monad. *)
  module With_errors : Generic_on_monad with type 'a t := 'a t
                                         and type 'a elt := 'a elt
                                         and module M := Or_error
  ;;
end

(** [Container0] is a generic interface for arity-0 traversable
    containers. *)
module type Container0 = sig
  (** [t] is the type of the container. *)
  type t
  (** [elt] is the type of elements. *)
  type elt

  include Generic_container with type 'a t := t and type 'a elt := elt
  include Mappable.S0_container with type t := t and type elt := elt
end

(** [Container1] is a generic interface for arity-1 traversable
    containers.  It also incldues [My_container.Extensions1]. *)
module type Container1 = sig
  (** ['a t] is the type of the container, parametrised over the
      element type ['a]. *)
  type 'a t

  (** [On_monad] implements monadic folding and mapping operators for
      a given monad [M], including arity-1 specific operators. *)
  module On_monad
    : functor (M : Monad.S)
      -> On_monad1 with type 'a t := 'a t and module M := M
  (** [With_errors] is shorthand for [On_monad (Or_error)]. *)
  module With_errors : On_monad1 with type 'a t := 'a t
                                  and module M := Or_error
  ;;

  include Generic_container with type 'a t := 'a t
                             and type 'a elt := 'a
                             and module On_monad := On_monad
                             and module With_errors := With_errors
  ;;

  include Mappable.S1_container with type 'a t := 'a t
  include Mappable.Extensions1 with type 'a t := 'a t

  (** [With_elt (Elt)] demotes this [Container1] to a [Container0] by
     fixing the element type to that mentioned in [Elt]. *)
  module With_elt
    : functor (Elt : Equal.S)
      -> Container0 with type t := Elt.t t and type elt := Elt.t
  ;;
end

(** [Traversable] contains things to export in [Traversable.mli]. *)
module type Traversable = sig
  module type Generic = Generic
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

  (** [Helpers] contains utility functions for building traversals.

      Functions beginning [proc_variant] are useful for building
      traversal functions on top of Variantslib's [map] function.  The
      function [proc_field] is useful for building fold-map functions
      on top of Fieldslib's [fold] function. *)
  module Helpers : functor (M : Monad.S) -> sig
    (** [traversal] is shorthand for a traversal function over [M]. *)
    type 'a traversal = ('a -> 'a M.t)


    (** [proc_variant0 f variant] lifts a traversal [f] over a
       Variantslib nullary variant constructor [variant]. *)
    val proc_variant0
      :  unit traversal
      -> 'cont Base.Variant.t
      -> 'cont M.t
    ;;

    (** [proc_variant1 f variant a] lifts a traversal [f] over a
       Variantslib unary variant constructor [variant] with argument
       [a]. *)
    val proc_variant1
      :  'a traversal
      -> ('a -> 'cont) Base.Variant.t
      -> 'a
      -> 'cont M.t
    ;;

    (** [proc_variant2 f variant a b] lifts a traversal [f] over a
       Variantslib binary variant constructor [variant] with arguments
       [a] and [b]. *)
    val proc_variant2
      : ('a * 'b) traversal
      -> ('a -> 'b -> 'cont) Base.Variant.t
      -> 'a
      -> 'b
      -> 'cont M.t
    ;;

    (** [proc_variant3 f variant a b c] lifts a traversal [f] over a
       Variantslib ternary variant constructor [variant] with
       arguments [a], [b], and [c]. *)
    val proc_variant3
      :  ('a * 'b * 'c) traversal
      -> ('a -> 'b -> 'c -> 'cont) Base.Variant.t
      -> 'a
      -> 'b
      -> 'c
      -> 'cont M.t
    ;;

    (** [proc_field f state field container original] lifts a
       traversal [f] to a form comparible with Fieldslib's [fold]
       function. *)
    val proc_field
      :  'elt traversal
      -> 'cont M.t
      -> ([> `Set_and_create], 'cont, 'elt) Field.t_with_perm
      -> 'cont M.t
    ;;
  end
end
