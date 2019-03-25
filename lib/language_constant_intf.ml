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

(** Language abstraction layer: constant analysis

    This module contains the interface act languages must implement
    for constant analysis, [Basic]; the expanded interface the rest of
    act gets, [S]; and a functor from one to the other, [Make]. *)

open Core_kernel

(** [Basic] is the interface act languages must implement for
    constant analysis. *)
module type Basic = sig
  (** [t] is the type of constants. *)
  type t [@@deriving compare, eq, sexp]

  (** Languages must supply a pretty-printer for their constants. *)
  include Pretty_printer.S with type t := t

  (** Languages must supply a Quickcheck generator for their
     constants. *)
  include Quickcheck.S with type t := t

  (** [of_int] lifts an integer to a constant. *)
  val of_int : int -> t
end

(** [S] is an expanded interface onto an act language's constant
    analysis. *)
module type S = sig
  include Basic

  (** [zero] is a constant representing numeric zero. *)
  val zero : t
end
