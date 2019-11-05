(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

(** [Basic] is the standard input signature for functors creating sanitiser
    passes. *)
module type Basic = sig
  module Lang : Act_language.Definition_types.S
  (** [Lang] provides language-specific functionality. *)

  module Ctx : Ctx_types.S with module Lang := Lang
  (** [Ctx] is the sanitiser's state monad, specialised over [Lang]. *)
end

(** [S] is the standard signature for sanitiser passes. *)
module type S = sig
  type t
  (** Type of pass subject. *)

  type 'a ctx
  (** Type of sanitiser context. *)

  val run : t -> t ctx
  (** [run x] runs this sanitiser pass over [x]. *)
end
