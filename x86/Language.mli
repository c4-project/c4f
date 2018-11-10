open Core
open Lib

(** Top-level language modules for x86 *)

(** [AttFrontend] is a parser/lexer combination for the AT&T syntax of
   x86 assembly, as emitted by compilers like gcc. *)
module AttFrontend : LangFrontend.Intf with type ast = Ast.t

(** [S] is the signature of language modules over the X86 AST. *)
module type S = sig
  include Dialect.S
  include PP.Printer
  include
    Language.S
    with type Constant.t = Ast.Operand.t
     and type Location.t = Ast.Location.t
     and type Instruction.t = Ast.Instruction.t
     and type Statement.t = Ast.Statement.t
     and type Symbol.t = string

  (** [make_jump_operand jsym] expands a jump symbol [jsym] to the
      correct abstract syntax for this version of x86. *)
  val make_jump_operand : string -> Ast.Operand.t
end

(** [Att] is a language description for the AT&T dialect of x86. *)
module ATT : S

(** [Intel] is a language description for the Intel dialect of x86. *)
module Intel : S

(** [Herd7] is a language description for the Herd7 dialect of x86. *)
module Herd7 : S

(** [of_dialect] gets the correct [S] module for a dialect. *)
val of_dialect : Dialect.t -> (module S)

(** [frontend_of_dialect] gets the correct frontend module for a dialect. *)
val frontend_of_dialect : Dialect.t -> (module LangFrontend.Intf with type ast = Ast.t) Or_error.t
