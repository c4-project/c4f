%{
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

open Core

module X86 = X86Base
open X86
%}

%token EOF
%token EOL
%token <X86Base.reg> ARCH_REG
%token <string> DIRECTIVE
%token <string> SYMB_REG
%token <string> ATT_NUM
%token <string> INTEL_NUM
%token <string> ATT_HEX
%token <string> INTEL_HEX
%token <string> NAME

%token SEMI COMMA LBRK RBRK
%token LPAR RPAR COLON
/* Instruction tokens */

%token  IT_XOR IT_OR IT_ADD  IT_MOV  IT_MOVB IT_MOVW IT_MOVL IT_MOVQ IT_MOVT IT_MOVSD IT_DEC  IT_CMP  IT_CMOVC  IT_INC  IT_JMP
%token  IT_LOCK  IT_XCHG   IT_LFENCE  IT_SFENCE  IT_MFENCE
%token  IT_SETNB IT_JE IT_JNE
%token  IT_CMPXCHG

%type <X86Base.pseudo list> main
%start  main

%%

main:
  | stm_list EOF { $1 }

semi_opt:
  | SEMI { () }
  | { () }

stm_list:
  | list(stm) {$1}

stm:
  | directive EOL {$1}
  | instr_option EOL {$1}
  | error EOL { raise (X86Base.ParseError($loc, X86Base.Statement)) }

directive:
  | DIRECTIVE separated_list(COMMA, directive_arg) { Nop }

directive_arg:
  | NAME { () }
  | operand { () }

instr_option:
  | NAME COLON instr_option { Label ($1,$3) }
  | option(instr)      { match $1 with | Some x -> Instruction x | None -> Nop}


reg:
| SYMB_REG { Symbolic_reg $1 }
| ARCH_REG { $1 }

instr:
  | IT_XOR   effaddr  COMMA  operand
    {I_XOR ($2,$4)}
  | IT_OR   effaddr  COMMA  operand
    {I_OR ($2,$4)}
  | IT_ADD   effaddr  COMMA  operand
    {I_ADD ($2,$4)}
  | IT_MOV   effaddr  COMMA  operand
    {I_MOV ($2,$4)}
  | IT_MOVB   effaddr  COMMA  operand
    {I_MOVB ($2,$4)}
  | IT_MOVW   effaddr  COMMA  operand
    {I_MOVW ($2,$4)}
  | IT_MOVL   effaddr  COMMA  operand
    {I_MOVL ($2,$4)}
  | IT_MOVQ   effaddr  COMMA  operand
    {I_MOVQ ($2,$4)}
  | IT_MOVT   effaddr  COMMA  operand
    {I_MOVT ($2,$4)}
  | IT_MOVSD
    {I_MOVSD}
  | IT_DEC   effaddr
    {I_DEC $2}
  | IT_CMP   effaddr COMMA   operand
    {I_CMP ($2,$4)}
  | IT_CMOVC reg COMMA  effaddr
    {I_CMOVC ($2, $4)}
  | IT_INC   effaddr
    {I_INC $2}
  | IT_JMP  NAME
    {I_JMP $2}
  | IT_JE NAME
    {I_JCC(C_EQ, $2)}
  | IT_JNE NAME
    {I_JCC(C_NE, $2)}
  | IT_LOCK semi_opt instr
    {I_LOCK $3 }
  | IT_XCHG   effaddr COMMA effaddr
    { I_XCHG ($2,$4)}
  | IT_CMPXCHG effaddr COMMA reg
    { I_CMPXCHG ($2,$4)}
  | IT_LFENCE
      { I_LFENCE}
  | IT_SFENCE
      { I_SFENCE}
  | IT_MFENCE
      { I_MFENCE}
  | IT_SETNB effaddr {I_SETNB $2 }

effaddr:
  | rm32  {Effaddr_rm32 $1}

rm32:
  |  reg {Rm32_reg $1}
  |  LPAR reg RPAR {Rm32_deref $2}
  |  LBRK reg RBRK {Rm32_deref $2}
  |  LBRK NAME RBRK {Rm32_abs (Constant.Symbolic ($2,0))}
  |  LBRK INTEL_NUM RBRK {Rm32_abs (Constant.Concrete $2)}

num:
  | ATT_NUM   { Int.of_string $1 }
  | INTEL_NUM { Int.of_string $1 }
  | ATT_HEX   { Int.of_string ("0x" ^ $1) }
  | INTEL_HEX { Int.of_string ("0x" ^ $1) }

operand:
  | effaddr {Operand_effaddr $1}
  | num {Operand_immediate $1} /* enough ? */
  | error COMMA { raise (X86Base.ParseError($loc, X86Base.Operand)) }
