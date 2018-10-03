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
open Utils

type reg =
  | EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP | EIP
  | AX | BX | CX | DX
  | AL | BL | CL | DL
  | AH | BH | CH | DH
  | ZF | SF | CF

module RegTable =
  StringTable.Make
    (struct
      type t = reg
      let table =
        [ EAX, "EAX"
        ; EBX, "EBX"
        ; ECX, "ECX"
        ; EDX, "EDX"
        ; ESI, "ESI"
        ; EDI, "EDI"
        ; EBP, "EBP"
        ; ESP, "ESP"
        ; EIP, "EIP"
        (* Flag registers *)
        ; ZF,  "ZF"
        ; SF,  "SF"
        ; CF,  "CF"
        (* 16-bit registers *)
        ; AX,  "AX"
        ; BX,  "BX"
        ; CX,  "CX"
        ; DX,  "DX"
        (* 8-bit low registers *)
        ; AL,  "AL"
        ; BL,  "BL"
        ; CL,  "CL"
        ; DL,  "DL"
        (* 8-bit high registers *)
        ; AH,  "AH"
        ; BH,  "BH"
        ; CH,  "CH"
        ; DH,  "DH"
        ]
    end)

(*
 * Displacements
 *)

type disp =
  | DispSymbolic of string
  | DispNumeric of int

let fold_map_disp_symbols ~f ~init =
  function
  | DispSymbolic s ->
     Tuple2.map_snd ~f:(fun x -> DispSymbolic x) (f init s)
  | DispNumeric  k -> (init, DispNumeric k)

(*
 * Indices
 *)

type index =
  | Unscaled of reg
  | Scaled of reg * int

(*
 * Memory addresses
 *)

type indirect =
  { in_seg    : reg option
  ; in_disp   : disp option
  ; in_base   : reg option
  ; in_index  : index option
  }

let in_zero () =
  { in_seg    = None
  ; in_disp   = None
  ; in_base   = None
  ; in_index  = None
  }

let in_base_only r = { (in_zero ()) with in_base = Some r }
let in_disp_only d = { (in_zero ()) with in_disp = Some d }

let fold_map_indirect_symbols ~f ~init indirect =
  (match indirect.in_disp with
   | Some d ->
      Tuple2.map_snd ~f:(fun x -> { indirect with in_disp = Some x })
                     (fold_map_disp_symbols ~f ~init d)
   | None   -> (init, indirect))

(*
 * Locations
 *)

type location =
  | LocIndirect of indirect
  | LocReg of reg

let fold_map_location_symbols ~f ~init =
  function
  | LocReg r -> (init, LocReg r)
  | LocIndirect ind ->
     Tuple2.map_snd ~f:(fun x -> LocIndirect x)
                    (fold_map_indirect_symbols ~f ~init ind)

(*
 * Operators
 *)

type bop =
  | BopPlus
  | BopMinus

(*
 * Operands
 *)

type operand =
  | OperandLocation of location
  | OperandImmediate of disp
  | OperandString of string
  | OperandBop of operand * bop * operand

let rec fold_map_operand_symbols ~f ~init =
  function
  | OperandLocation l ->
     Tuple2.map_snd ~f:(fun x -> OperandLocation x)
                    (fold_map_location_symbols ~f ~init l)
  | OperandImmediate d ->
     Tuple2.map_snd ~f:(fun x -> OperandImmediate x)
                    (fold_map_disp_symbols ~f ~init d)
  | OperandString s -> (init, OperandString s)
  | OperandBop (l, b, r) ->
     let (init, l') = fold_map_operand_symbols ~f ~init l in
     let (init, r') = fold_map_operand_symbols ~f ~init r in
     (init, OperandBop (l', b, r'))

let rec fold_map_operand_locations ~f ~init =
  function
  | OperandLocation l ->
     Tuple2.map_snd ~f:(fun x -> OperandLocation x)
                    (f init l)
  | OperandImmediate d -> (init, OperandImmediate d)
  | OperandString s -> (init, OperandString s)
  | OperandBop (l, b, r) ->
     let (init, l') = fold_map_operand_locations ~f ~init l in
     let (init, r') = fold_map_operand_locations ~f ~init r in
     (init, OperandBop (l', b, r'))

(*
 * Prefixes
 *)

type prefix =
  | PreLock

(*
 * Sizes
 *)

type size =
  | X86SByte
  | X86SWord
  | X86SLong

(*
 * Conditions
 *)

type inv_condition =
  [ `Above
  | `AboveEqual
  | `Below
  | `BelowEqual
  | `Carry
  | `Equal
  | `Greater
  | `GreaterEqual
  | `Less
  | `LessEqual
  | `Overflow
  | `Parity
  | `Sign
  | `Zero
  ]

type condition =
  [ inv_condition
  | `Not of inv_condition
  | `CXZero
  | `ECXZero
  | `ParityEven
  | `ParityOdd
  ]


module InvConditionTable =
  StringTable.Make
    (struct
      type t = inv_condition
      let table =
        [ `Above       , "a"
        ; `AboveEqual  , "ae"
        ; `Below       , "b"
        ; `BelowEqual  , "be"
        ; `Carry       , "c"
        ; `Equal       , "e"
        ; `Greater     , "g"
        ; `GreaterEqual, "ge"
        ; `Less        , "l"
        ; `LessEqual   , "le"
        ; `Overflow    , "o"
        ; `Parity      , "p"
        ; `Sign        , "s"
        ; `Zero        , "z"
        ]
    end)

(** [build_inv_condition (ic, s) builds, for an invertible condition
   C, string table entries for C and NC. *)
let build_inv_condition (ic, s) =
  [ ((ic :> condition), s)
  ; (`Not ic, "n" ^ s)
  ]

module ConditionTable =
  StringTable.Make
    (struct
      type t = condition
      let table =
        List.bind ~f:build_inv_condition InvConditionTable.table
        @
        [ `CXZero    , "cxz"
        ; `ECXZero   , "ecxz"
        ; `ParityEven, "pe"
        ; `ParityOdd , "po"
        ]
    end)

(*
 * Opcodes
 *)

type sizable_opcode =
  [ `Add
  | `Mov
  | `Pop
  | `Push
  | `Sub
  ]

module SizableOpcodeTable =
  StringTable.Make
    (struct
      type t = sizable_opcode
      let table =
        [ `Add,  "add"
        ; `Mov,  "mov"
        ; `Pop,  "pop"
        ; `Push, "push"
        ; `Sub,  "sub"
        ]
    end)

module ATTSizeTable =
  StringTable.Make
    (struct
      type t = size
      let table =
        [ X86SByte, "b"
        ; X86SWord, "w"
        ; X86SLong, "l"
        ]
    end)

module ATTSizedOpcodeTable =
  StringTable.Make
    (struct
      type t = (sizable_opcode * size)

      let table =
        List.map
          ~f:(fun ((op, ops), (sz, szs)) -> ((op, sz), ops^szs))
          (List.cartesian_product SizableOpcodeTable.table
                                  ATTSizeTable.table)
    end)

type basic_opcode =
  [ sizable_opcode
  | `Leave
  | `Mfence
  | `Nop
  | `Ret
  ]

module BasicOpcodeTable =
  StringTable.Make
    (struct
      type t = basic_opcode
      let table =
        (SizableOpcodeTable.table
         :> (basic_opcode, string) List.Assoc.t)
        @
        [ `Leave,  "leave"
        ; `Mfence, "mfence"
        ; `Nop,    "nop"
        ; `Ret,    "ret"
        ]
    end)

type opcode =
  | OpBasic of basic_opcode
  | OpSized of sizable_opcode * size
  | OpJump of condition option
  | OpDirective of string
  | OpUnknown of string

module JumpTable =
  StringTable.Make
    (struct
      type t = condition option

      (* Jump instructions are always jC for some condition C, except
         jmp. *)
      let f (x, s) = (Some x, "j" ^ s)
      let table = (None, "jmp") :: List.map ~f ConditionTable.table
    end)

(*
 * Instructions
 *)

type instruction =
  { prefix   : prefix option
  ; opcode   : opcode
  ; operands : operand list
  }

let fold_map_instruction_symbols ~f ~init ins =
  Tuple2.map_snd ~f:(fun x -> { ins with operands = x })
                 (List.fold_map ~f:(fun init -> fold_map_operand_symbols ~f ~init)
                                ~init
                                ins.operands)

let fold_map_instruction_locations ~f ~init ins =
  Tuple2.map_snd ~f:(fun x -> { ins with operands = x })
                 (List.fold_map ~f:(fun init -> fold_map_operand_locations ~f ~init)
                                ~init
                                ins.operands)

(*
 * Statements
 *)

type statement =
  | StmInstruction of instruction
  | StmLabel of string
  | StmNop

(** [t] is the type of an X86 abstract syntax tree, containing the
    specific X86 syntax dialect and a list of statements. *)
type t =
  { syntax  : X86Dialect.t
  ; program : statement list
  }

let fold_map_statement_symbols ~f ~init =
  function
  | StmInstruction i ->
     Tuple.T2.map_snd ~f:(fun x -> StmInstruction x)
                      (fold_map_instruction_symbols ~f ~init i)
  | StmLabel l ->
     Tuple.T2.map_snd ~f:(fun x -> StmLabel x)
                      (f init l)
  | StmNop -> (init, StmNop)

let fold_map_statement_instructions ~f ~init =
  function
  | StmInstruction i ->
     Tuple.T2.map_snd ~f:(fun x -> StmInstruction x)
                      (f init i)
  | StmLabel l -> (init, StmLabel l)
  | StmNop -> (init, StmNop)
