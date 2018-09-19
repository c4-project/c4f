(** [Frontend] is a parser/lexer combination for the AT&T syntax of
   x86 assembly, as emitted by compilers like gcc. *)
module Frontend : (LangFrontend.S with type ast = X86Ast.statement list)
