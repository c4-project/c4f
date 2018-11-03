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

  module Abs : Abstract.S

  (** [abs_type x] gets the abstract type of [x]. *)
  val abs_type : t -> Abs.t
end

(** [Basic_enum] is a common signature for things that have an
   abstract type, implementing [Abstract.S_enum], that can be
   queried. *)
module type Basic_enum = sig
  module Abs : Abstract.S_enum

  include Basic with module Abs := Abs
end

(** [S] is an extended signature for things that have an
    abstract type, implementing [Abstract.S], that can be queried. *)
module type S = sig
  include Basic

  (* This space left to expand on later. *)
end

(** [S_enum] is an extended signature for things that have an abstract
   type, implementing [Abstract.S_enum]. that can be queried. *)
module type S_enum = sig
  include Basic_enum
  include S with type t := t and module Abs := Abs

  (** [has_abs_type t x] returns [true] if [x] has abstract type
      [t]. *)
  val has_abs_type : Abs.t -> t -> bool

  (** [abs_type_in set x] returns [true] if [x]'s abstract type is a
     member of [set]. *)
  val abs_type_in : Abs.Set.t -> t -> bool
end

(** [Abstractable] is the signature that gets re-exported in
    [Abstractable.mli]. *)
module type Abstractable = sig
  module type Basic = Basic
  module type Basic_enum = Basic_enum
  module type S = S
  module type S_enum = S_enum

  (** [Make] extends a [Basic] to an [S]. *)
  module Make
    : functor (B : Basic)
      -> S with type t := B.t and module Abs := B.Abs
  ;;

  (** [Make_enum] extends a [Basic_enum] to an [S_enum]. *)
  module Make_enum
    : functor (B : Basic_enum)
      -> S_enum with type t := B.t and module Abs := B.Abs
  ;;
end
