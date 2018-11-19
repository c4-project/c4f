(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
   LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
   OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** [Abstract_instruction] contains types and utilities for abstracted
    instructions. *)

(** [t] is an abstracted instruction. *)
type t =
  | Arith   (** arithmetic *)
  | Call    (** calling-convention related instructions *)
  | Compare (** comparison *)
  | Fence   (** memory fence *)
  | Jump    (** conditional or unconditional jump *)
  | Logical (** logical operation *)
  | Move    (** move *)
  | Nop     (** no operation *)
  | Return  (** jump to caller *)
  | Rmw     (** read-modify-write *)
  | Stack   (** stack resizing and pointer manipulation *)
  | Other   (** known, but doesn't fit in these categories *)
  | Unknown (** unclassified instruction *)
;;

(* Why do we have a separate [Return] type, instead of classing it
   as [Call] or [Jump]?  Two reasons:

   - It has semantics roughly in between both;

   - It makes it easier for us to translate returns to
   end-of-program jumps in sanitisation.  *)

include Abstract_base.S_enum with type t := t
