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

(* We don't open Core at the toplevel because Menhir generates exceptions that
   are ok in the standard library, but deprecated in Core. *)
 open Ast
%}

%token EOF
%token EOL
%token PLUS
%token MINUS
%token DOLLAR
%token <Ast.Reg.t> ATT_REG
%token <string> STRING
%token <string> NUM
%token <string> ATT_HEX
%token <string> NAME
%token <string> GAS_TYPE

%token COMMA
%token LPAR RPAR COLON
/* Instruction tokens */

%token  IT_LOCK

%type <Ast.statement list> main
%start  main

%%

main:
  | stm_list EOF { $1 }

stm_list:
  | list(stm) { $1 }

stm:
  | option(instr) EOL { Core.Option.value ~default:StmNop $1 }
  | label { StmLabel $1 }
  | error { raise (Base.ParseError({at = $sloc; because = "unknown statement error" })) }

prefix:
  | IT_LOCK { PreLock }

label:
  NAME COLON { $1 }

opcode:
  | NAME { Core.Option.(
	     (Core.String.chop_prefix $1 ~prefix:"." >>| (fun f -> OpDirective f))
	     |> first_some
	          (JumpTable.of_string $1 >>| (fun j -> OpJump j))
	     |> first_some
	          (ATTSizedOpcodeTable.of_string $1 >>| (fun (x, y) -> OpSized (x, y)))
	     |> first_some
	          (BasicOpcodeTable.of_string $1 >>| (fun o -> OpBasic o))
	     |> value ~default:(OpUnknown $1)
	   )
	 }

instr:
  | prefix opcode separated_list (COMMA, operand)
           { StmInstruction
               { prefix = Some $1
               ; opcode = $2
               ; operands = $3
               }
           }
    (* lock cmpxchgl %eax, %ebx *)
  | opcode separated_list (COMMA, operand)
         { StmInstruction
               { prefix = None
               ; opcode = $1
               ; operands = $2
               }
         }

(* Binary operator *)
bop:
  | PLUS { BopPlus }
  | MINUS { BopMinus }

(* Base/index/scale triple *)
bis:
  | ATT_REG
    { in_base_only $1 }
    (* (%eax) *)
  | option(ATT_REG) COMMA ATT_REG
         { { (in_zero ()) with in_base = $1;
                                       in_index = Some (Unscaled $3) } }
    (* (%eax, %ebx)
       (    , %ebx) *)
  | option(ATT_REG) COMMA ATT_REG COMMA k
         { { (in_zero ()) with in_base = $1;
                                      in_index = Some (Scaled ($3, $5)) } }
    (* (%eax, %ebx, 2)
       (    , %ebx, 2) *)

(* Segment:displacement *)
segdisp:
  | disp { (None, $1) }
  | separated_pair(ATT_REG, COLON, disp) { Core.(Tuple2.map_fst ~f:Option.some $1) }

(* Memory access: base/index/scale, displacement, or both *)
indirect:
  | delimited(LPAR, bis, RPAR) { $1 }
    (* (%eax, %ebx, 2) *)
  | segdisp delimited(LPAR, bis, RPAR) { { $2 with in_seg = fst $1; in_disp = Some (snd $1) } }
    (* -8(%eax, %ebx, 2) *)
  | segdisp { in_seg_disp $1 }
    (* 0x4000 *)

location:
  | indirect {LocIndirect $1}
    (* -8(%eax, %ebx, 2) *)
  | ATT_REG {LocReg $1}
    (* %eax *)
  | ATT_REG error
    { raise (Base.ParseError(
		 { at = $sloc
		 ; because = "unexpected item following register"
		 }
	    ))
    }

(* Memory displacement *)
disp:
  | k    { DispNumeric $1 }
  | NAME { DispSymbolic $1 }

operand:
  | prim_operand bop operand { OperandBop($1,$2,$3) }
  | prim_operand { $1 }
  | error { raise (Base.ParseError(
		       { at = $sloc
		       ; because = "unknown error in operand"
		       }
		  ))
	  }

prim_operand:
  | DOLLAR disp {OperandImmediate $2}
    (* $10 *)
  | STRING {OperandString $1}
    (* @function *)
  | GAS_TYPE { OperandType $1 }
    (* "Hello, world!" *)
  | location {OperandLocation $1}

(* Numeric constant: hexadecimal or decimal *)
k:
  | ATT_HEX    { Core.Int.of_string ("0x" ^ $1) }
    (* 0xDEADBEEF *)
  | NUM        { Core.Int.of_string $1 }
    (* 42 *)
