open Core
open Lib

(** Top-level language modules for x86 *)

(** [AttFrontend] is a parser/lexer combination for the AT&T syntax of
   x86 assembly, as emitted by compilers like gcc. *)
module AttFrontend : LangFrontend.Intf with type ast = Ast.t

(** [Intf] is the type of language modules over the X86 AST. *)
module type Intf = sig
  include Dialect.Intf
  include PP.Printer
  include
    Language.Intf
    with type Constant.t = Ast.Operand.t
     and type Location.t = Ast.location
     and type Instruction.t = Ast.instruction
     and type Statement.t = Ast.statement

  (** [make_jump_operand jsym] expands a jump symbol [jsym] to the
      correct abstract syntax for this version of x86. *)
  val make_jump_operand : string -> Ast.Operand.t
end

(** [Att] is a language description for the AT&T dialect of x86. *)
module ATT : Intf

(** [Intel] is a language description for the Intel dialect of x86. *)
module Intel : Intf

(** [Herd7] is a language description for the Herd7 dialect of x86. *)
module Herd7 : Intf

(** [lang_of_dialect] gets the correct [Intf] module for a dialect. *)
val lang_of_dialect : Dialect.t -> (module Intf)

(** [frontend_of_dialect] gets the correct frontend module for a dialect. *)
val frontend_of_dialect : Dialect.t -> (module LangFrontend.Intf with type ast = Ast.t) Or_error.t
