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

(** [Basic] is a common signature for things that have an abstract
   type, implementing [Abstract.S], that can be queried. *)
module type Basic = sig
  (** [t] is the concrete type. *)
  type t

  module Abs : Abstract_base.S

  (** [abstract x] gets the abstract form of [x]. *)
  val abstract : t -> Abs.t
end

(** [S] is an extended signature for things that have an
    abstract type, implementing [Abstract.S], that can be queried. *)
module type S = sig
  include Basic

  (** [abs_kind x] gets the abstract kind of [x]. *)
  val abs_kind : t -> Abs.Kind.t

  (** [has_abs_kind t x] returns [true] if [x] has abstract kind
      [t]. *)
  val has_abs_kind : Abs.Kind.t -> t -> bool

  (** [abs_kind_in set x] returns [true] if [x]'s abstract kind is a
     member of [set]. *)
  val abs_kind_in : Abs.Kind.Set.t -> t -> bool
end

(** [Abstractable] is the signature that gets re-exported in
    [Abstractable.mli]. *)
module type Abstractable = sig
  module type Basic = Basic
  module type S = S

  (** [Make] extends a [Basic] to an [S]. *)
  module Make
    : functor (B : Basic)
      -> S with type t := B.t and module Abs := B.Abs
  ;;
end
