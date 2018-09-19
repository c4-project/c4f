open Core

type ('p, 'l) error =
  | Parse of 'p
  | Lex of 'l

module type S = sig
  type ast
  type token
  type perr
  type lerr

  val pp_perr : Format.formatter -> perr -> unit
  val pp_lerr : Format.formatter -> lerr -> unit

  val lex : Lexing.lexbuf -> token
  val parse : (Lexing.lexbuf -> token)
              -> Lexing.lexbuf
              -> (ast, (perr, lerr) error) result
end
