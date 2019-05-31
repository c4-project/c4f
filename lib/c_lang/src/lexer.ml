(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Parser
module S = Sedlexing

let tr_name typedefs s =
  match s with
  | "volatile" ->
      VOLATILE
  | "auto" ->
      AUTO
  | "char" ->
      CHAR
  | "int" ->
      INT
  | "long" ->
      LONG
  | "float" ->
      FLOAT
  | "double" ->
      DOUBLE
  | "short" ->
      SHORT
  | "struct" ->
      STRUCT
  | "union" ->
      UNION
  | "enum" ->
      ENUM
  | "signed" ->
      SIGNED
  | "unsigned" ->
      UNSIGNED
  | "break" ->
      BREAK
  | "continue" ->
      CONTINUE
  | "return" ->
      RETURN
  | "sizeof" ->
      SIZEOF
  | "do" ->
      DO
  | "while" ->
      WHILE
  | "for" ->
      FOR
  | "goto" ->
      GOTO
  | "if" ->
      IF
  | "else" ->
      ELSE
  | "switch" ->
      SWITCH
  | "case" ->
      CASE
  | "default" ->
      DEFAULT
  | "void" ->
      VOID
  (* Litmus extensions *)
  | "exists" ->
      LIT_EXISTS
  | "locations" ->
      LIT_LOCATIONS
  (* Others *)
  | x when Set.mem typedefs x ->
      TYPEDEF_NAME x
  | x ->
      IDENTIFIER x

let digit = [%sedlex.regexp? '0' .. '9']

let hex = [%sedlex.regexp? ascii_hex_digit]

let alpha = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']

let num = [%sedlex.regexp? Plus digit]

let c_id_start = [%sedlex.regexp? alpha | '_']

let c_id_middle = [%sedlex.regexp? c_id_start | digit | '.']

let c_id = [%sedlex.regexp? c_id_start, Star c_id_middle]

let ws = [%sedlex.regexp? Chars " \t\r"]

let eol = [%sedlex.regexp? '\n']

let rec token (typedefs : Set.M(String).t) (lexbuf : S.lexbuf) : token =
  match%sedlex lexbuf with
  | Plus ws ->
      token typedefs lexbuf
  | eol ->
      token typedefs lexbuf
  | "/*" ->
      Act_utils.Lex_utils.skip_c_comment lexbuf ;
      token typedefs lexbuf
  | "//" ->
      Act_utils.Lex_utils.skip_line lexbuf ;
      token typedefs lexbuf
  | Opt '-', num ->
      INT_LIT (Core.Int.of_string (S.Utf8.lexeme lexbuf))
  | ';' ->
      SEMI
  | ',' ->
      COMMA
  | ':' ->
      COLON
  | '=' ->
      EQ
  | "*=" ->
      STAR_EQ
  | "/=" ->
      DIV_EQ
  | "%=" ->
      MOD_EQ
  | "+=" ->
      ADD_EQ
  | "-=" ->
      SUB_EQ
  | "<<=" ->
      SHL_EQ
  | ">>=" ->
      SHR_EQ
  | "&=" ->
      AND_EQ
  | "^=" ->
      XOR_EQ
  | "|=" ->
      PIPE_EQ
  | "||" ->
      LOR
  | "&&" ->
      LAND
  | '^' ->
      XOR
  | '|' ->
      PIPE
  | '&' ->
      AND
  | "==" ->
      EQ_OP
  | "!=" ->
      NEQ_OP
  | "<" ->
      LT
  | ">" ->
      GT
  | "<=" ->
      LE
  | ">=" ->
      GE
  | "<<" ->
      SHL
  | ">>" ->
      SHR
  | '+' ->
      ADD
  | '-' ->
      SUB
  | '*' ->
      STAR
  | '/' ->
      DIV
  | '%' ->
      MOD
  | '~' ->
      NOT
  | '!' ->
      LNOT
  | '(' ->
      LPAR
  | ')' ->
      RPAR
  | '{' ->
      LBRACE
  | '}' ->
      RBRACE
  | '[' ->
      LBRACK
  | ']' ->
      RBRACK
  | '?' ->
      QUESTION
  | "..." ->
      DOTS
  | "->" ->
      ARROW
  | "++" ->
      ADDADD
  | "--" ->
      SUBSUB
  | '.' ->
      DOT
  | '#' ->
      Act_utils.Frontend.lex_error
        "C preprocessor directives aren't supported directly." lexbuf
  (* Litmus extensions *)
  | "/\\" ->
      LIT_AND
  | "\\/" ->
      LIT_OR
  | c_id ->
      tr_name typedefs (S.Utf8.lexeme lexbuf)
  | eof ->
      EOF
  | _ ->
      Act_utils.Frontend.lex_error
        ("Unexpected char: " ^ S.Utf8.lexeme lexbuf)
        lexbuf
