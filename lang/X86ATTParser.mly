%{

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

open Core
%}

%token EOF
%token EOL
%token PLUS
%token MINUS
%token DOLLAR
%token <X86Ast.reg> ATT_REG
%token <string> STRING
%token <string> NUM
%token <string> ATT_HEX
%token <string> NAME

%token COMMA
%token LPAR RPAR COLON
/* Instruction tokens */

%token  IT_LOCK

%type <X86Ast.statement list> main
%start  main

%%

main:
  | stm_list EOF { $1 }

stm_list:
  | list(stm) { $1 }

stm:
  | option(instr) EOL { Option.value ~default:StmNop $1 }
  | label { X86Ast.StmLabel $1 }
  | error { raise (X86Base.ParseError($loc, X86Base.Statement)) }

prefix:
  | IT_LOCK { PreLock }

label:
  NAME COLON { $1 }

instr:
  | prefix NAME separated_list (COMMA, operand)
           { X86Ast.StmInstruction
               { prefix = Some $1
               ; opcode = $2
               ; operands = $3
               }
           }
    (* lock cmpxchgl %eax, %ebx *)
  | NAME separated_list (COMMA, operand)
         { match String.chop_prefix $1 ~prefix:"." with
           | Some dir_name ->
              X86Ast.StmDirective
                { dir_name
                ; dir_ops  = $2
                }
           | None ->
              X86Ast.StmInstruction
                { prefix = None
                ; opcode = $1
                ; operands = $2
                }
         }

(* Binary operator *)
bop:
  | PLUS { X86Ast.BopPlus }
  | MINUS { X86Ast.BopMinus }

(* Base/index/scale triple *)
bis:
  | LPAR ATT_REG RPAR
    { { (X86Base.in_zero ()) with in_base = Some $2 } }
    (* (%eax) *)
  | LPAR option(ATT_REG) COMMA ATT_REG RPAR
         { { (X86Base.in_zero ()) with in_base = $2;
                                       in_index = Some (Unscaled $4) } }
    (* (%eax, %ebx)
       (    , %ebx) *)
  | LPAR option(ATT_REG) COMMA ATT_REG COMMA k RPAR
         { { (X86Base.in_zero ()) with in_base = $2;
                                       in_index = Some (Scaled ($4, $6)) } }
    (* (%eax, %ebx, 2)
       (    , %ebx, 2) *)

(* Memory access: base/index/scale, displacement, or both *)
indirect:
  | bis { $1 }
    (* (%eax, %ebx, 2) *)
  | disp bis { { $2 with in_disp = Some $1 } }
    (* -8(%eax, %ebx, 2) *)
  | disp { { (X86Base.in_zero ()) with in_disp = Some $1 } }
    (* 0x4000 *)

(* Memory displacement *)
disp:
  | k    { X86Ast.DispNumeric $1 }
  | NAME { X86Ast.DispSymbolic $1 }

operand:
  | prim_operand bop operand { X86Ast.OperandBop($1,$2,$3) }
  | prim_operand { $1 }
  | error { raise (X86Base.ParseError($sloc, X86Base.Operand)) }

prim_operand:
  | DOLLAR disp {X86Ast.OperandImmediate $2}
    (* $10 *)
  | STRING {X86Ast.OperandString $1}
    (* "Hello, world!" *)
  | ATT_REG {X86Ast.OperandReg $1}
    (* %eax *)
  | indirect {X86Ast.OperandIndirect $1}
    (* -8(%eax, %ebx, 2) *)

(* Numeric constant: hexadecimal or decimal *)
k:
  | ATT_HEX    { Int.of_string ("0x" ^ $1) }
    (* 0xDEADBEEF *)
  | NUM        { Int.of_string $1 }
    (* 42 *)
