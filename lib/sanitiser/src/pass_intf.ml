(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** [Basic] is the standard input signature for functors creating sanitiser
    passes. *)
module type Basic = sig
  (** [Lang] provides language-specific functionality. *)
  module Lang : Act_language.Definition.S

  (** [Ctx] is the sanitiser's state monad, specialised over [Lang]. *)
  module Ctx : Ctx.S with module Lang := Lang

  (** [Program_container] is the container used to hold the one or more
      programs being sanitised. *)
  module Program_container : Travesty.Traversable.S1
end

(** [S] is the standard signature for sanitiser passes. *)
module type S = sig
  (** Type of pass subject. *)
  type t

  (** Type of sanitiser context. *)
  type 'a ctx

  val run : t -> t ctx
  (** [run x] runs this sanitiser pass over [x]. *)
end
