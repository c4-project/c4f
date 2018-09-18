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
    open LexMisc
    open X86Parser
    module X86 = X86Base
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
| "0x" { raise (error ("Malformed hex constant: " ^ Lexing.lexeme lexbuf) lexbuf) }
| (hexnum as x) 'h' { INTEL_HEX x }
| '%' (name as name)
      { match X86.parse_reg name with
        | Some r -> ATT_REG r
        | None -> raise (error ("Invalid register: " ^ Lexing.lexeme lexbuf) lexbuf)
      }
| ',' { COMMA }
| '(' { LPAR }
| ')' { RPAR }
| '[' { LBRK }
| ']' { RBRK }
| ':' { COLON }
| '+' { PLUS }
| '-' { MINUS }
| '"' { read_string (Buffer.create 17) lexbuf }
| name as x { x
              |> X86.parse_reg
              |> Option.value_map ~default:(NAME x) ~f:(fun f -> INTEL_REG f)
            }
| eof { EOF }
| _ { raise (error ("Unexpected char: " ^ Lexing.lexeme lexbuf) lexbuf) }

(* per 'Real World OCaml' *)
and read_string buf
  = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '0' { Buffer.add_char buf '\x00'; read_string buf lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _ { raise (error ("Invalid string character: " ^ Lexing.lexeme lexbuf) lexbuf) }

{
let token lexbuf =
   let tok = token lexbuf in
   if O.debug then begin
     Printf.eprintf
       "%a: Lexed '%s'\n"
       Pos.pp_pos2
       (lexeme_start_p lexbuf,lexeme_end_p lexbuf)
       (lexeme lexbuf)
   end ;
   tok
end
}

