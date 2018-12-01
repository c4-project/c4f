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

module type Generic_types = sig
  (** [t] is the type of container to map over. *)
  type 'a t
  (** [elt] is the type of element inside the container. *)
  type 'a elt
end

(** [Generic] is the parent interface of [S0] and [S1], abstracting
    over the arity of the mappable type. *)
module type Generic = sig
  include Generic_types

  (** [map c ~f] maps [f] over every [t] in [c]. *)
  val map : 'a t -> f:('a elt -> 'b elt) -> 'b t
end

(** [S0] is the signature of an arity-0 mappable type.

    Functions mapped over arity-0 types must preserve the element
   type.  *)
module type S0 = sig
  (** [t] is the type that we map over. *)
  type t
  (** [elt] is the type that the mapping function receives and
     returns. *)
  type elt

  include Generic with type 'a t := t and type 'a elt := elt
end

(** [S0_container] is the signature of an arity-0 mappable container. *)
module type S0_container = sig
  include S0
  include Container.S0 with type t := t and type elt := elt
end

(** [S1] is the signature of an arity-1 mappable type.

    Functions mapped over arity-1 types may change the element type. *)
module type S1 = sig
  (** [t] is the type of the container to map over. *)
  type 'a t

  include Generic with type 'a t := 'a t and type 'a elt := 'a
end

(** [S1_container] is the signature of an arity-1 mappable container. *)
module type S1_container = sig
  include S1
  include Container.S1 with type 'a t := 'a t
end

(** [Extensions1] describes various extensions of arity-1 mappable
   containers. *)
module type Extensions1 = sig
   (** [t] is the type of the container to map over. *)
  type 'a t

  include My_container.Extensions1 with type 'a t := 'a t

  (** [right_pad ~padding xs] pads every list in xs with [padding],
      ensuring all lists have equal length. *)
  val right_pad : padding:'a -> 'a list t -> 'a list t
end

(** [Mappable] contains things to export in [Mappable.mli]. *)
module type Mappable = sig
  module type Generic = Generic
  module type Generic_types = Generic_types
  module type S0 = S0
  module type S1 = S1
  module type S0_container = S0_container
  module type S1_container = S1_container
  module type Extensions1 = Extensions1

  (** [Extend1] implements [Extensions1] for an arity-1 mappable
     container. *)
  module Extend1
    : functor (S : S1_container) -> Extensions1 with type 'a t := 'a S.t
  ;;
end
