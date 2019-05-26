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

(** Abstract type of context passed into filter stages. *)

type 'aux t

(** {2 Constructors} *)

val make : aux:'aux -> input:Input.t -> output:Output.t -> 'aux t
(** [make ~aux ~input ~output] makes a filter context. *)

(** {2 Accessors} *)

val aux : 'aux t -> 'aux
(** [aux ctx] gets the auxiliary input information passed through [ctx]. *)

val input : _ t -> Input.t
(** [input ctx] gets the input descriptor passed through [ctx]. *)

val output : _ t -> Output.t
(** [output ctx] gets the output descriptor passed through [ctx]. *)

(** {2 Transforming the auxiliary input} *)

module On_aux : Travesty.Traversable.S1 with type 'a t := 'a t
