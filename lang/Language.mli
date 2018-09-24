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

(** High-level interface to languages supported by act *)

open Core

(** [name] enumerates all languages, and dialects thereof, supported by act. *)
type name =
  | X86 of X86Ast.syntax

(** [name_of_sexp] converts from an S-expression to a [name]. *)
val name_of_sexp : Sexp.t -> name
(** [sexp_of_name] converts from a [name] to an S-expression. *)
val sexp_of_name : name -> Sexp.t

(** [pp_name ?show_sublang f l] pretty-prints language name [l] onto
   formatter [f].  If [show_sublang] is true (the default), we also
   print details about any sub-language (eg. AT&T/Intel syntax for
   X86). *)
val pp_name : ?show_sublang:bool -> Format.formatter -> name -> unit

(** [S] is the signature implemented by Litmus languages. *)
module type S = sig
  (** [statement] is the type of statements in the language. *)
  type statement

  (** [location] is the type of initialisable and checkable
     locations. *)
  type location

  (** [constant] is the type of constants used in initialisation and
     checks. *)
  type constant

  (** [name] is the name of the signature. *)
  val name : name

  (** [pp_statement f s] pretty-prints statement [s] on formatter [f]. *)
  val pp_statement : Format.formatter -> statement -> unit

  (** [pp_location f l] pretty-prints location [l] on formatter [f]. *)
  val pp_location : Format.formatter -> location -> unit

  (** [pp_constant f k] pretty-prints constant [k] on formatter [f]. *)
  val pp_constant : Format.formatter -> constant -> unit

  (*
   * Analyses and heuristics
   *)

  (** [nop] builds a no-op instruction. *)
  val nop : unit -> statement

  (** [is_directive stm] decides whether [stm] appears to be an
     assembler directive. *)
  val is_directive : statement -> bool

  (** [is_program_boundary stm] decides whether [stm] appears to mark
      a boundary between two Litmus programs. *)
  val is_program_boundary : statement -> bool
end
