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

(** [abs_instruction] is an abstracted instruction. *)
type abs_instruction =
  | AIJump of string list
  | AIMove
  | AINop
  | AICall
  | AIStack
  | AIOther

(** [abs_statement] is an abstracted statement. *)
type abs_statement =
  | ASDirective of string
  | ASInstruction of abs_instruction
  | ASBlank
  | ASLabel of string
  | ASOther

(** [BaseS] is the top-level signature that must be implemented by act
   languages (as part of [S]). *)
module type BaseS = sig
  (** [name] is the name of the language. *)
  val name : name

  (** [is_program_label l] decides whether symbol [l] is a label
     marking the start of a program. *)
  val is_program_label : string -> bool
end

(** [StatementS] is the signature that must be implemented by
    act languages in regards to their statement types. *)
module type StatementS = sig
  type t

  (** Languages must supply a pretty-printer for their statements. *)
  include Core.Pretty_printer.S with type t := t

  (*
   * Transformations
   *)

  (** [map_statement_ids f stm] maps an identifier rewriting functon
     [f] over [stm]. *)
  val map_ids : f:(string -> string) -> t -> t

  (*
   * Analyses and heuristics
   *)

  (** [nop] builds a no-op instruction. *)
  val nop : unit -> t

  (** [statement_type stm] gets the abstract type of a statement. *)
  val statement_type : t -> abs_statement
end

module type LocationS = sig
  type t
  include Core.Pretty_printer.S with type t := t
end

module type ConstantS = sig
  type t
  include Core.Pretty_printer.S with type t := t
end

(** [S] is the signature that Litmus languages must implement. *)
module type S = sig
  include BaseS
  module Statement : StatementS
  module Location : LocationS
  module Constant : ConstantS
end

(** [Intf] is the user-facing interface module for act languages.
    Usually, you can get an [Intf] by applying the functor [Make]
    onto a bare-bones language [S]. *)
module type Intf = sig
  include BaseS

  module Statement : sig
    include StatementS

    (** [is_directive stm] decides whether [stm] appears to be an
     assembler directive. *)
    val is_directive : t -> bool

    (** [is_nop stm] decides whether [stm] appears to be a NOP. *)
    val is_nop : t -> bool

    (** [instruction_type stm] gets the abstract type of an instruction.
     If [stm] isn't an instruction, it returns [None].  *)
    val instruction_type : t -> abs_instruction option

    (** [is_program_boundary stm] decides whether [stm] appears to mark
      a boundary between two Litmus programs. *)
    val is_program_boundary : t -> bool
  end

  module Location : sig
    include LocationS
  end

  module Constant : sig
    include ConstantS
  end
end

(** [Make] builds a module satisfying [Intf] from a module satisfying [S]. *)
module Make : functor (M : S) ->
              (Intf with type Statement.t = M.Statement.t
                     and type Location.t  = M.Location.t
                     and type Constant.t  = M.Constant.t)
