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
   See: https://sourceware.org/binutils/docs/as/Symbol-Intro.html#Symbol-Intro

   NB: We forbid symbols beginning with '$' and '.' to prevent ambiguity against
   directives.  It may be that we need to permit these later on---the as manual
   doesn't rule them out. *)
let nameprf = (alpha|'_')
let namechr = (nameprf|digit|'$' | '.')
let name  = nameprf namechr*

let num = digit+
let hexnum = hex+

rule token = parse
| [' ''\t''\r'] { token lexbuf }
| '\n'      { new_line lexbuf; EOL }
| "/*"      { LU.skip_c_comment lexbuf ; token lexbuf }
| '#'       { LU.skip_c_line_comment lexbuf ; EOL }
| '.' (name as d) { DIRECTIVE d }
| '-' ? num as x { INTEL_NUM x }
| '$' ('-'? num as x) { ATT_NUM x }
| "0x" (hexnum as x) { ATT_HEX x }
| "0x" { raise (error ("Malformed hex constant: " ^ Lexing.lexeme lexbuf) lexbuf) }
| (hexnum as x) 'h' { INTEL_HEX x }
| '%' (name as name) { SYMB_REG name }
| ';' { SEMI }
| ',' { COMMA }
| '(' { LPAR }
| ')' { RPAR }
| '[' { LBRK }
| ']' { RBRK }
| ':' { COLON }
| "add"|"ADD"   { IT_ADD }
| "xor"|"XOR"   { IT_XOR }
| "or"|"OR"   { IT_OR }
| "mov"|"MOV"   { IT_MOV }
| "movb"|"MOVB"   { IT_MOVB }
| "movw"|"MOVW"   { IT_MOVW }
| "movl"|"MOVL"   { IT_MOVL }
| "movq"|"MOVQ"   { IT_MOVQ }
| "movt"|"MOVT"   { IT_MOVT }
| "movsd"|"MOVSD"   { IT_MOVSD }
| "dec"|"DEC"   { IT_DEC }
| "cmp"|"CMP"   { IT_CMP }
| "cmovc"|"CMOVC"   { IT_CMOVC }
| "inc"|"INC"   { IT_INC }
| "jmp"|"JMP"   { IT_JMP }
| "je"|"JE"    { IT_JE }
| "jne"|"JNE"    { IT_JNE }
| "lock"|"LOCK"   { IT_LOCK }
| "xchg"|"XCHG"   { IT_XCHG }
| "cmpxchg"|"CMPXCHG"   { IT_CMPXCHG }
| "lfence"|"LFENCE"   { IT_LFENCE }
| "sfence"|"SFENCE"   { IT_SFENCE }
| "mfence"|"MFENCE"   { IT_MFENCE }
| "setnb"|"SETNB"       { IT_SETNB }
| name as x
  { match X86.parse_reg x with
    | Some r -> ARCH_REG r
    | None -> NAME x
  }
| eof { EOF }
| _ { raise (error ("Unexpected char: " ^ Lexing.lexeme lexbuf) lexbuf) }

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

