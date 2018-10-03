(** [Frontend] is a parser/lexer combination for the AT&T syntax of
   x86 assembly, as emitted by compilers like gcc. *)
module Frontend : LangFrontend.S with type ast = X86Ast.t

(** [Lang] is a language description for the AT&T syntax of x86. *)
module Lang : Language.Intf
  with type Constant.t = X86Ast.operand
   and type Location.t = X86Ast.location
   and type Instruction.t = X86Ast.instruction
   and type Statement.t = X86Ast.statement

