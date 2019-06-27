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

(** Sanitiser: warnings *)

open Base

include module type of Warn_intf
(** @inline *)

type 'elt t
(** Opaque type of warnings, parametrised over the element being warned
    about. *)

val program_name : _ t -> string
(** [program_name warn] gets the name of the program [warn] concerns. *)

val element : 'elt t -> 'elt
(** [element warn] gets the element [warn] concerns. *)

val body : _ t -> Info.t
(** [body warn] gets [warn]'s underlying [Info.t]. *)

val make : program_name:string -> element:'elt -> body:Info.t -> 'elt t
(** [make ~program_name ~element ~body] creates a warning. *)

(** [Make] produces a warnings module for the given language. *)
module Make (Elt : Act_language.Element.S) :
  S with type t = Elt.t t and type elt = Elt.t
