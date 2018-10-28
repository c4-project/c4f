(*
This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor
   (parts (c) 2010-2018 Institut National de Recherche en Informatique et
	                en Automatique, Jade Alglave, and Luc Maranget)

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
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

This file derives from the Herd7 project
(https://github.com/herd/herdtools7); its original attribution and
copyright notice follow. *)

(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

{
  module Make(O:LexUtils.Config) = struct
    open Core
    open Lexing
    open Lib.LangFrontend
    open ATTParser
    module LU = LexUtils.Make(O)
}
let digit = [ '0'-'9' ]
let hex   = [ '0'-'9' 'a'-'f' 'A'-'F' ]
let alpha = [ 'a'-'z' 'A'-'Z' ]

(* Symbol lexing
   See: https://sourceware.org/binutils/docs/as/Symbol-Intro.html#Symbol-Intro *)
let nameprf = (alpha|'_'|'.')
let namechr = (nameprf|digit| '$' | '.')
let name  = nameprf namechr*

let num = digit+
let hexnum = hex+

rule token = parse
| [' ''\t''\r'] { token lexbuf }
| '\n'      { new_line lexbuf; EOL }
| "/*"      { LU.skip_c_comment lexbuf ; token lexbuf }
| '#'       { LU.skip_c_line_comment lexbuf ; EOL }
| '-' ? num as x { NUM x }
| '$' { DOLLAR }
| "0x" (hexnum as x) { ATT_HEX x }
| "0x" { lex_error ("Malformed hex constant: " ^ Lexing.lexeme lexbuf) lexbuf }
| '%' (name as name)
      { match Ast.Reg.of_string_option name with
        | Some r -> ATT_REG r
        | None -> lex_error ("Invalid register: " ^ Lexing.lexeme lexbuf) lexbuf
      }
| ',' { COMMA }
| '(' { LPAR }
| ')' { RPAR }
| ':' { COLON }
| '+' { PLUS }
| '-' { MINUS }
| '"' { read_string (Buffer.create 17) lexbuf }
| "lock" { IT_LOCK }
| name as x { NAME x }
| '@' name as x { GAS_TYPE x }
| eof { EOF }
| _ { lex_error ("Unexpected char: " ^ Lexing.lexeme lexbuf) lexbuf }

(* per 'Real World OCaml' *)
and read_string buf
  = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' '0' { Buffer.add_char buf '\x00'; read_string buf lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _ { lex_error ("Invalid string character: " ^ Lexing.lexeme lexbuf) lexbuf }

{
end
}

