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

module Reg = struct
  module M = struct
    (* Ordered as in Intel manual *)
    type t =
      | AH | AL | AX | EAX
      | BH | BL | BX | EBX
      | CH | CL | CX | ECX
      | DH | DL | DX | EDX
      | BP | EBP
      | SI | ESI
      | DI | EDI
      | SP | ESP
      | CS | DS | SS | ES | FS | GS
      | CF | PF | AF | ZF | SF | OF
      | EIP
    [@@deriving enum, sexp]
    ;;

    let table =
      [ AH , "AH"
      ; AL , "AL"
      ; AX , "AX"
      ; EAX, "EAX"
      ; BH , "BH"
      ; BL , "BL"
      ; BX , "BX"
      ; EBX, "EBX"
      ; CH , "CH"
      ; CL , "CL"
      ; CX , "CX"
      ; ECX, "ECX"
      ; DH , "DH"
      ; DL , "DL"
      ; DX , "DX"
      ; EDX, "EDX"
      ; BP , "BP"
      ; EBP, "EBP"
      ; SI , "SI"
      ; ESI, "ESI"
      ; DI , "DI"
      ; EDI, "EDI"
      ; SP , "SP"
      ; ESP, "ESP"
      ; CS , "CS"
      ; DS , "DS"
      ; SS , "SS"
      ; ES , "ES"
      ; FS , "FS"
      ; GS , "GS"
      ; CF , "CF"
      ; PF , "PF"
      ; AF , "AF"
      ; ZF , "ZF"
      ; SF , "SF"
      ; OF , "OF"
      ; EIP, "EIP"
      ]
    ;;
  end

  include M
  include Enum.ExtendTable (M)

  type kind =
    | Gen8 of [`Low | `High]
    | Gen16
    | Gen32
    | Segment
    | Flags
    | IP
  ;;

  let kind_of : t -> kind = function
    | EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP -> Gen32
    | AX | BX | CX | DX | SI | DI | BP | SP -> Gen16
    | AH | BH | CH | DH -> Gen8 `High
    | AL | BL | CL | DL -> Gen8 `Low
    | CS | DS | SS | ES | FS | GS -> Segment
    | CF | PF | AF | ZF | SF | OF -> Flags
    | EIP -> IP
  ;;
end

(*
 * Displacements
 *)

type disp =
  | DispSymbolic of string
  | DispNumeric of int
[@@deriving sexp]

let fold_map_disp_symbols ~f ~init =
  function
  | DispSymbolic s ->
     Tuple2.map_snd ~f:(fun x -> DispSymbolic x) (f init s)
  | DispNumeric  k -> (init, DispNumeric k)

(*
 * Indices
 *)

type index =
  | Unscaled of Reg.t
  | Scaled of Reg.t * int
[@@deriving sexp]

let fold_map_index_registers ~f ~init = function
  | Unscaled r -> Tuple2.map_snd ~f:(fun x -> Unscaled x) (f init r)
  | Scaled (r, k) -> Tuple2.map_snd ~f:(fun x -> Scaled (x, k)) (f init r)
;;

(*
 * Memory addresses
 *)

let fold_opt (g : 'a -> 'b -> ('a * 'b)) (v : 'a) (ro : 'b option) : ('a * 'b option) =
  Option.value_map ~default:(v, None)
    ~f:(fun x -> Tuple2.map_snd ~f:Option.some (g v x)) ro
;;

module Indirect = struct
  type t =
    { seg    : Reg.t option
    ; disp   : disp option
    ; base   : Reg.t option
    ; index  : index option
    }
  [@@deriving sexp, fields]

  let make ?base ?seg ?disp ?index () = { seg; disp; base; index };;

  let fold_map
      ~init
      ?(seg=Tuple2.create) ?(disp=Tuple2.create) ?(base=Tuple2.create) ?(index=Tuple2.create)
      indirect =
    Fields.Direct.fold
      indirect
      ~init:(init, make ())
      ~seg:(fun (state, ind) fld _ cseg ->
          Tuple2.map_snd ~f:(Field.fset fld ind) (fold_opt seg state cseg))
      ~disp:(fun (state, ind) fld _ cdisp ->
          Tuple2.map_snd ~f:(Field.fset fld ind) (fold_opt disp state cdisp))
      ~base:(fun (state, ind) fld _ cbase ->
          Tuple2.map_snd ~f:(Field.fset fld ind) (fold_opt base state cbase))
      ~index:(fun (state, ind) fld _ cindex ->
          Tuple2.map_snd ~f:(Field.fset fld ind) (fold_opt index state cindex))
  ;;

  let fold_map_symbols ~f ~init indirect =
    fold_map
      ~init
      ~disp:(fun init -> fold_map_disp_symbols ~f ~init)
      indirect
  ;;

  let fold_map_registers ~f ~init indirect =
    fold_map
      ~init
      ~seg:f
      ~base:f
      ~index:(fun init -> fold_map_index_registers ~f ~init)
      indirect
  ;;
end

(*
 * Locations
 *)

type location =
  | LocIndirect of Indirect.t
  | LocReg of Reg.t
[@@deriving sexp]

let fold_map_location_symbols ~f ~init =
  function
  | LocReg r -> (init, LocReg r)
  | LocIndirect ind ->
     Tuple2.map_snd ~f:(fun x -> LocIndirect x)
                    (Indirect.fold_map_symbols ~f ~init ind)

let fold_map_location_registers ~f ~init = function
  | LocReg r ->
    Tuple2.map_snd ~f:(fun x -> LocReg x) (f init r)
  | LocIndirect i ->
    Tuple2.map_snd ~f:(fun x -> LocIndirect x)
      (Indirect.fold_map_registers ~f ~init i)
;;

(*
 * Operators
 *)

type bop =
  | BopPlus
  | BopMinus
[@@deriving sexp]

(*
 * Operands
 *)

type operand =
  | OperandLocation of location
  | OperandImmediate of disp
  | OperandString of string
  | OperandType of string
  | OperandBop of operand * bop * operand
[@@deriving sexp]

let rec fold_map_operand_symbols ~f ~init =
  function
  | OperandLocation l ->
     Tuple2.map_snd ~f:(fun x -> OperandLocation x)
                    (fold_map_location_symbols ~f ~init l)
  | OperandImmediate d ->
     Tuple2.map_snd ~f:(fun x -> OperandImmediate x)
                    (fold_map_disp_symbols ~f ~init d)
  | OperandString s -> (init, OperandString s)
  | OperandType ty -> (init, OperandType ty)
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
  | OperandType ty -> (init, OperandType ty)
  | OperandBop (l, b, r) ->
     let (init, l') = fold_map_operand_locations ~f ~init l in
     let (init, r') = fold_map_operand_locations ~f ~init r in
     (init, OperandBop (l', b, r'))

(*
 * Prefixes
 *)

type prefix =
  | PreLock
[@@deriving sexp]

(*
 * Sizes
 *)

type size =
  | SByte
  | SWord
  | SLong
[@@deriving sexp]

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
[@@deriving sexp]

type condition =
  [ inv_condition
  | `Not of inv_condition
  | `CXZero
  | `ECXZero
  | `ParityEven
  | `ParityOdd
  ]
[@@deriving sexp]


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
  | `Call
  | `Cmp
  | `Mov
  | `Pop
  | `Push
  | `Ret
  | `Sub
  | `Xor
  ]
[@@deriving sexp]

module SizableOpcodeTable =
  StringTable.Make
    (struct
      type t = sizable_opcode
      let table =
        [ `Add,  "add"
        ; `Call, "call"
        ; `Cmp,  "cmp"
        ; `Mov,  "mov"
        ; `Pop,  "pop"
        ; `Push, "push"
        ; `Ret,  "ret"
        ; `Sub,  "sub"
        ; `Xor,  "xor"
        ]
    end)

module ATTSizeTable =
  StringTable.Make
    (struct
      type t = size
      let table =
        [ SByte, "b"
        ; SWord, "w"
        ; SLong, "l"
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
  ]
[@@deriving sexp]

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
        ]
    end)

type opcode =
  | OpBasic of basic_opcode
  | OpSized of sizable_opcode * size
  | OpJump of condition option
  | OpDirective of string
  | OpUnknown of string
[@@deriving sexp]

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
[@@deriving sexp]

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
[@@deriving sexp]

(** [t] is the type of an X86 abstract syntax tree, containing the
    specific X86 syntax dialect and a list of statements. *)
type t =
  { syntax  : Dialect.t
  ; program : statement list
  }
[@@deriving sexp, fields]

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
