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
%token PLUS
%token MINUS
%token DOLLAR
%token <X86Base.reg> ATT_REG
%token <X86Base.reg> INTEL_REG
%token <string> STRING
%token <string> NUM
%token <string> ATT_HEX
%token <string> INTEL_HEX
%token <string> NAME

%token COMMA LBRK RBRK
%token LPAR RPAR COLON
/* Instruction tokens */

%token  IT_LOCK

%type <X86Base.statement list> main
%start  main

%%

main:
  | stm_list EOF { $1 }

stm_list:
  | list(stm) { $1 }

stm:
  | option(instr) EOL { Option.value ~default:Stm_nop $1 }
  | label { Stm_label $1 }
  | error { raise (X86Base.ParseError($loc, X86Base.Statement)) }

reg:
| ATT_REG { $1 }
| INTEL_REG { $1 }

prefix:
  | IT_LOCK { Pre_Lock }

label:
  NAME COLON { $1 }

instr:
  | prefix NAME separated_list (COMMA, operand)
           { Stm_instruction
               { prefix = Some $1
               ; opcode = $2
               ; operands = $3
               }
           }
  | NAME separated_list (COMMA, operand)
         { if String.is_prefix $1 ~prefix:"."
           then Stm_directive
                  { dir_name = $1
                  ; dir_ops  = $2
                  }
           else Stm_instruction
             { prefix = None
             ; opcode = $1
             ; operands = $2
             }
         }

effaddr:
  | rm32  {Effaddr_rm32 $1}

rm32:
  |  reg {Rm32_reg $1}
  |  NAME {Rm32_abs (Constant.Symbolic ($1,0))}
  |  LPAR ATT_REG RPAR {Rm32_deref $2}
  |  LBRK INTEL_REG RBRK {Rm32_deref $2}
  |  LBRK NAME RBRK {Rm32_abs (Constant.Symbolic ($2,0))}
  |  LBRK NUM RBRK {Rm32_abs (Constant.Concrete $2)}

bop:
  | PLUS { Bop_plus }
  | MINUS { Bop_minus }

operand:
  | prim_operand bop operand { Operand_bop($1,$2,$3) }
  | prim_operand { $1 }
  | error { raise (X86Base.ParseError($sloc, X86Base.Operand)) }

prim_operand:
  | effaddr {Operand_effaddr $1}
  | k {Operand_immediate $1}
  | STRING {Operand_string $1}

k:
  | ATT_HEX    { Int.of_string ("0x" ^ $1) }
  | INTEL_HEX  { Int.of_string ("0x" ^ $1) }
  | NUM        { Int.of_string $1 }
