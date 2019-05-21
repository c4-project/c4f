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

(* We don't open Core_kernel at the toplevel because Menhir generates exceptions that
   are ok in the standard library, but deprecated in Core_kernel. *)
 open Ast
%}

%token EOF
%token EOL
%token PLUS
%token MINUS
%token DOLLAR
%token <Reg.t> ATT_REG
%token <string> STRING
%token <string> NUM
%token <string> ATT_HEX
%token <string> NAME
%token <string> GAS_TYPE

%token COMMA
%token LPAR RPAR COLON
/* Instruction tokens */

%token  IT_LOCK

%type <Ast.t> main
%start  main

%%

main:
  | program=list(stm) EOF
    {
      Ast.make
        ~dialect:(Act_common.Id.of_string "att")
        ~program
	()
    }

stm:
  | maybe_instr=option(instr) EOL
    {
      Base.Option.value_map
	maybe_instr
	~f:Statement.instruction ~default:Statement.Nop
    }
  | label=label { Statement.Label label }

prefix:
  | IT_LOCK { PreLock }

label:
  symbol=NAME COLON { symbol }

opcode:
  | name=NAME { Opcode.of_string name}

instr:
  | prefix=prefix opcode=opcode operands=separated_list(COMMA, operand)
    { Instruction.make ~prefix ~opcode ~operands () }
    (* lock cmpxchgl %eax, %ebx *)
  | opcode=opcode operands=separated_list(COMMA, operand)
    { Instruction.make ~opcode ~operands () }

(* Binary operator *)
bop:
  | PLUS { Bop.Plus }
  | MINUS { Bop.Minus }

(* Base/index/scale triple *)
bis:
  | base=ATT_REG
    { (Some base, None) }
    (* (%eax) *)
  | maybe_base=option(ATT_REG) COMMA index=ATT_REG
         { (maybe_base, Some (Index.Unscaled index)) }
    (* (%eax, %ebx)
       (    , %ebx) *)
  | maybe_base=option(ATT_REG) COMMA index=ATT_REG COMMA scale=k
         { (maybe_base, Some (Index.Scaled (index, scale))) }
    (* (%eax, %ebx, 2)
       (    , %ebx, 2) *)

(* Segment:displacement *)
segdisp:
  | disp=disp { (None, disp) }
  | segdisp=separated_pair(ATT_REG, COLON, disp)
    { Core_kernel.(Tuple2.map_fst ~f:Option.some segdisp) }

(* Memory access: base/index/scale, displacement, or both *)
indirect:
  | bis=delimited(LPAR, bis, RPAR)
    {
      let (base, index) = bis in
      Indirect.make ?base ?index ()
    }
    (* (%eax, %ebx, 2) *)
  | segdisp=segdisp bis=delimited(LPAR, bis, RPAR)
    {
      let (seg, disp) = segdisp in
      let (base, index) = bis in
      Indirect.make ?seg ~disp ?base ?index ()
    }
    (* -8(%eax, %ebx, 2) *)
  | segdisp=segdisp
    {
      let (seg, disp) = segdisp in
      Indirect.make ?seg ~disp ()
    }
    (* 0x4000 *)

location:
  | indirect=indirect { Location.Indirect indirect }
    (* -8(%eax, %ebx, 2) *)
  | reg=ATT_REG { Location.Reg reg }
    (* %eax *)

(* Memory displacement *)
disp:
  | number=k    { Disp.Numeric  number }
  | symbol=NAME { Disp.Symbolic symbol }

operand:
  | l=prim_operand op=bop r=operand { Operand.bop l op r }
  | p=prim_operand { p }

prim_operand:
  | DOLLAR immediate=disp { Operand.immediate immediate }
    (* $10 *)
  | string=STRING { Operand.string string }
    (* @function *)
  | typ=GAS_TYPE { Operand.typ typ }
    (* "Hello, world!" *)
  | location=location { Operand.location location }

(* Numeric constant: hexadecimal or decimal *)
k:
  | hex=ATT_HEX    { Core_kernel.Int.of_string ("0x" ^ hex) }
    (* 0xDEADBEEF *)
  | dec=NUM        { Core_kernel.Int.of_string dec }
    (* 42 *)
