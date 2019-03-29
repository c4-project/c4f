(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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

(** Sanitiser: warnings: interfaces *)

open Base

(** [S] is the interface to the warnings emitted by the sanitiser. *)
module type S = sig
  (** [Lang] is the language to whose elements we're attaching warnings. *)
  module Lang : Language.S

  (** [elt] is the type of elements being warned about. *)
  type elt =
    | Instruction of Lang.Instruction.t
    | Location of Lang.Location.t
    | Operands of Lang.Instruction.t
    | Statement of Lang.Statement.t
    | Symbol of Lang.Symbol.t

  (** [t] is the opaque type of warnings. *)
  type t

  val program_name : t -> string
  (** [program_name warn] gets the name of the program [warn] concerns. *)

  val element : t -> elt
  (** [element warn] gets the element [warn] concerns. *)

  val body : t -> Info.t
  (** [body warn] gets [warn]'s underlying [Info.t]. *)

  val make : program_name:string -> element:elt -> body:Info.t -> t
  (** [make ~program_name ~element ~body] creates a warning. *)

  val not_understood : unit -> Info.t
  (** [not_understood ()] creates a standard warning body for an element
      that wasn't understood by act. *)

  val erroneous : Error.t -> Info.t
  (** [erroneous reason] creates a standard warning body for an element that
      act thinks is erroneous, for reason [reason]. *)

  include Pretty_printer.S with type t := t
end

module type Sanitiser_warn = sig
  module type S = S

  (** [Make] produces a warnings module for the given language. *)
  module Make (Lang : Language.S) : S with module Lang := Lang
end
