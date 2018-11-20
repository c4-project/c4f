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

(** [Inherit] contains helper signatures for building functors that
    let abstract data types 'inherit' properties of one of their
    components.

    This is based on the pattern used in [Base.Comparable]. *)

(** [S] describes a parent type [t], a component type [c], and a
    function [component] for getting the [c] of a [t]. *)
module type S = sig
  (** [t] is the main type. *)
  type t
  (** [c] is the type of inner components. *)
  type c

  (** [component x] gets the [c]-typed component of [x]. *)
  val component : t -> c
end

(** [S_partial] describes a parent type [t], an optional component
   type [c], and a function [component_opt] for getting the [c] of a
   [t], if one exists. *)
module type S_partial = sig
  (** [t] is the main type. *)
  type t
  (** [c] is the type of inner components. *)
  type c

  (** [component_opt x] tries to get the [c]-typed component of [x]. *)
  val component_opt : t -> c option
end

(** [Make_partial] converts an [S] into an [S_partial] that always
    returns [Some (component x)] for [component_opt x]. *)
module Make_partial
  : functor (I : S)
    -> S_partial with type t = I.t and type c = I.c
;;
