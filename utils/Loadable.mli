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

(** [Loadable] contains signatures for abstract data types that can
    be loaded from a file or string, and functors for adding
    convenience functions to such types for loading from a variety of
    sources. *)

open Core_kernel

include module type of Loadable_intf

module Make (B : Basic) : S with type t := B.t
(** [Make] extends a [Basic] into an [S]. *)

(** {2 Loading from standard formats} *)

module Of_sexpable (B : Sexpable.S) : S with type t := B.t
(** [Of_sexpable] extends a [Sexpable] into an [S]; the added
    methods load S-expressions. *)

(** {2 Chaining} *)

module Make_chain (B : Basic) (C : Basic_chain with type src := B.t)
  : S with type t := C.dst
(** Makes a new {{!S}S} from chaining a basic loadable [B] to a
    transformation function described in [C]. *)
