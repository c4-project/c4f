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
%}

%token EOF
%token EOL
%token PLUS
%token MINUS
%token DOLLAR
%token <X86Base.reg> ATT_REG
%token <string> STRING
%token <string> NUM
%token <string> ATT_HEX
%token <string> NAME

%token COMMA
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

prefix:
  | IT_LOCK { Pre_Lock }

label:
  NAME COLON { $1 }

instr:
  | prefix NAME separated_list (COMMA, operand)
           { X86Base.Stm_instruction
               { prefix = Some $1
               ; opcode = $2
               ; operands = $3
               }
           }
  | NAME separated_list (COMMA, operand)
         { if String.is_prefix $1 ~prefix:"."
           then X86Base.Stm_directive
                  { dir_name = $1
                  ; dir_ops  = $2
                  }
           else X86Base.Stm_instruction
             { prefix = None
             ; opcode = $1
             ; operands = $2
             }
         }

bop:
  | PLUS { X86Base.Bop_plus }
  | MINUS { X86Base.Bop_minus }

bis:
  | LPAR ATT_REG RPAR
    { { (X86Base.in_zero ()) with in_base = Some $2 } }
  | LPAR option(ATT_REG) COMMA ATT_REG RPAR
         { { (X86Base.in_zero ()) with in_base = $2;
                                       in_index = Some $4 } }
  | LPAR option(ATT_REG) COMMA option(ATT_REG) COMMA k RPAR
         { { (X86Base.in_zero ()) with in_base = $2;
                                       in_index = $4;
                                       in_scale = Some $6 } }

indirect:
  | bis { $1 }
  | disp bis { { $2 with in_disp = Some $1 } }
  | disp { { (X86Base.in_zero ()) with in_disp = Some $1 } }

disp:
  | k    { X86Base.DispNumeric $1 }
  | NAME { X86Base.DispSymbolic $1 }

operand:
  | prim_operand bop operand { X86Base.Operand_bop($1,$2,$3) }
  | prim_operand { $1 }
  | error { raise (X86Base.ParseError($sloc, X86Base.Operand)) }

prim_operand:
  | DOLLAR disp {X86Base.Operand_immediate $2}
  | STRING {X86Base.Operand_string $1}
  | ATT_REG {X86Base.Operand_reg $1}
  | indirect {X86Base.Operand_indirect $1}

k:
  | ATT_HEX    { Int.of_string ("0x" ^ $1) }
  | NUM        { Int.of_string $1 }
