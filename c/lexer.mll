(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2014-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

{
open Core_kernel
open Parser
open Lexing

(* Compiled efficiently by the next version of ocaml *)
let tr_name typedefs s = match s with
| "volatile" -> VOLATILE
| "auto" -> AUTO
| "char" -> CHAR
| "int" -> INT
| "long" -> LONG
| "float" -> FLOAT
| "double" -> DOUBLE
| "short" -> SHORT
| "struct" -> STRUCT
| "union" -> UNION
| "enum" -> ENUM
| "signed" -> SIGNED
| "unsigned" -> UNSIGNED
| "break" -> BREAK
| "continue" -> CONTINUE
| "return" -> RETURN
| "sizeof" -> SIZEOF
| "do" -> DO
| "while" -> WHILE
| "for" -> FOR
| "goto" -> GOTO
| "if"   -> IF
| "else" -> ELSE
| "switch" -> SWITCH
| "case" -> CASE
| "default" -> DEFAULT
| "void" -> VOID
(* Litmus extensions *)
| "exists" -> LIT_EXISTS
| "locations" -> LIT_LOCATIONS
(* Others *)
| x when String.Set.mem typedefs x -> TYPEDEF_NAME x
| x -> IDENTIFIER x
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let name = (alpha | '_') (alpha|digit|'_')*
let num = digit+

rule token typedefs = parse
| [' ''\t''\r']+ { token typedefs lexbuf }
| '\n' { new_line lexbuf ; token typedefs lexbuf }
| "/*" { Utils.Lex_utils.skip_c_comment lexbuf ; token typedefs lexbuf }
| "//" { Utils.Lex_utils.skip_c_line_comment lexbuf ; token typedefs lexbuf }
| '-' ? num as x { INT_LIT (Core.Int.of_string x) }
| ';' { SEMI }
| ',' { COMMA }
| ':' { COLON }

| '='   { EQ }
| "*="  { STAR_EQ }
| "/="  { DIV_EQ }
| "%="  { MOD_EQ }
| "+="  { ADD_EQ }
| "-="  { SUB_EQ }
| "<<=" { SHL_EQ }
| ">>=" { SHR_EQ }
| "&="  { AND_EQ }
| "^="  { XOR_EQ }
| "|="  { PIPE_EQ }

| "||" { LOR }
| "&&" { LAND }

| '^' { XOR }
| '|' { PIPE }
| '&' { AND }

| "==" { EQ_OP }
| "!=" { NEQ_OP }

| "<"  { LT }
| ">"  { GT }
| "<=" { LE }
| ">=" { GE }

| "<<" { SHL }
| ">>" { SHR }

| '+' { ADD }
| '-' { SUB }

| '*' { STAR }
| '/' { DIV }
| '%' { MOD }

| '~' { NOT }
| '!' { LNOT }

| '(' { LPAR }
| ')' { RPAR }
| '{' { LBRACE }
| '}' { RBRACE }
| '[' { LBRACK }
| ']' { RBRACK }

| '?' { QUESTION }
| "..." { DOTS }

| "->" { ARROW }
| "++" { ADDADD }
| "--" { SUBSUB }
| '.' {DOT}

| '#' { Utils.Frontend.lex_error
          ("C preprocessor directives aren't supported directly.")
          lexbuf
      }

(* Litmus extensions *)
| "/\\" { LIT_AND }
| "\\/" { LIT_OR }

| name as x { tr_name typedefs x }
| eof { EOF }
| _ { Utils.Frontend.lex_error ("Unexpected char: " ^ lexeme lexbuf) lexbuf }
