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

type syntax =
  | SynAtt
  | SynIntel

module SyntaxMap =
  StringTable.Make
    (struct
      type t = syntax
      let table =
        [ SynAtt  , "AT&T"
        ; SynIntel, "Intel"
        ]
    end)

let sexp_of_syntax syn =
  syn |> SyntaxMap.to_string_exn |> Sexp.Atom

let syntax_of_sexp =
  function
  | Sexp.Atom a as s ->
     begin
       match SyntaxMap.of_string a with
       | Some v -> v
       | None -> raise (Sexp.Of_sexp_error (failwith "expected x86 syntax name", s))
     end
  | s -> raise (Sexp.Of_sexp_error (failwith "expected x86 syntax, not a list", s))


let pp_syntax f syn =
  Format.pp_print_string f (Option.value ~default:"??" (SyntaxMap.to_string syn))

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

let pp_reg syn f reg =
  Format.pp_open_box f 0;
  if syn = SynAtt then Format.pp_print_char f '%';
  Format.pp_print_string f (RegTable.to_string_exn reg);
  Format.pp_close_box f ()

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

let pp_disp f = function
  | DispSymbolic s -> Format.pp_print_string f s
  | DispNumeric  0 -> ()
  | DispNumeric  k -> Format.pp_print_int    f k

(*
 * Indices
 *)

type index =
  | Unscaled of reg
  | Scaled of reg * int

let pp_index syn f =
  function
  | Unscaled r -> pp_reg syn f r
  | Scaled (r, i) -> Format.fprintf f
                                    "%a%s%d"
                                    (pp_reg syn) r
                                    (match syn with
                                     | SynAtt -> ","
                                     | SynIntel -> "*")
                                    i
(*
 * Memory addresses
 *)

type indirect =
  { in_seg    : reg option
  ; in_disp   : disp option
  ; in_base   : reg option
  ; in_index  : index option
  }

let fold_map_indirect_symbols ~f ~init indirect =
  (match indirect.in_disp with
   | Some d ->
      Tuple2.map_snd ~f:(fun x -> { indirect with in_disp = Some x })
                     (fold_map_disp_symbols ~f ~init d)
   | None   -> (init, indirect))

let pp_seg syn f =
  Format.fprintf f "%a:" (pp_reg syn)

let pp_bis_att f bo iso =
  match bo, iso with
  | None  , None -> ()
  | Some b, None ->
     Format.fprintf f "(%a)"
                    (pp_reg SynAtt) b
  | _     , Some i ->
     Format.fprintf f "(%a,%a)"
                    (MyFormat.pp_option ~pp:(pp_reg SynAtt)) bo
                    (pp_index SynAtt) i

let pp_indirect_att f {in_seg; in_disp; in_base; in_index} =
  MyFormat.pp_option f ~pp:(pp_seg SynAtt) in_seg;
  MyFormat.pp_option f ~pp:pp_disp in_disp;
  pp_bis_att f in_base in_index

let disp_positive =
  function
  | None -> false
  | Some (DispNumeric k) -> 0 < k
  | _ -> true

let pp_indirect_intel f {in_seg; in_disp; in_base; in_index} =
  Format.pp_open_box f 0;
  Format.pp_print_char f '[';

  (* seg:base+index*scale+disp *)

  MyFormat.pp_option f ~pp:(pp_seg SynIntel) in_seg;

  MyFormat.pp_option f ~pp:(pp_reg SynIntel) in_base;

  let plus_between_b_i = in_base <> None && in_index <> None in
  if plus_between_b_i then Format.pp_print_char f '+';

  MyFormat.pp_option f ~pp:(pp_index SynIntel) in_index;

  let plus_between_bis_d =
    (in_base <> None || in_index <> None)
    && disp_positive in_disp
  in
  if plus_between_bis_d then Format.pp_print_char f '+';

  MyFormat.pp_option f ~pp:pp_disp in_disp;

  Format.pp_print_char f ']';
  Format.pp_close_box f ()

let pp_indirect syn f ind =
  Format.pp_open_box f 0;
  if syn = SynAtt then pp_indirect_att f ind else pp_indirect_intel f ind;
  Format.pp_close_box f ()

(*
 * Operators
 *)

type bop =
  | BopPlus
  | BopMinus

let pp_bop f = function
  | BopPlus -> Format.pp_print_char f '+'
  | BopMinus -> Format.pp_print_char f '-'

(*
 * Operands
 *)

type operand =
  | OperandIndirect of indirect
  | OperandReg of reg
  | OperandImmediate of disp
  | OperandString of string
  | OperandBop of operand * bop * operand

let rec fold_map_operand_symbols ~f ~init =
  function
  | OperandIndirect i ->
     Tuple2.map_snd ~f:(fun x -> OperandIndirect x)
                    (fold_map_indirect_symbols ~f ~init i)
  | OperandReg r -> (init, OperandReg r)
  | OperandImmediate d ->
     Tuple2.map_snd ~f:(fun x -> OperandImmediate x)
                    (fold_map_disp_symbols ~f ~init d)
  | OperandString s -> (init, OperandString s)
  | OperandBop (l, b, r) ->
     let (init, l') = fold_map_operand_symbols ~f ~init l in
     let (init, r') = fold_map_operand_symbols ~f ~init r in
     (init, OperandBop (l', b, r'))

let string_escape =
  String.Escaping.escape_gen_exn
    ~escape_char:'\\'
    ~escapeworthy_map:[ '\x00', '0'
                      ; '"', '"'
                      ; '\\', '\\'
                      ]

let rec pp_operand syn f = function
  | OperandIndirect i -> pp_indirect syn f i
  | OperandReg r -> pp_reg syn f r
  | OperandImmediate d ->
     Format.pp_open_box f 0;
     if syn = SynAtt then Format.pp_print_char f '$';
     pp_disp f d;
     Format.pp_close_box f ();
  | OperandString s ->
     Format.fprintf f "\"%s\"" (Staged.unstage string_escape s)
  | OperandBop (l, b, r) ->
     Format.pp_open_box f 0;
     pp_operand syn f l;
     pp_bop f b;
     pp_operand syn f r;
     Format.pp_close_box f ()

let pp_comma f =
  Format.pp_print_char f ',';
  Format.pp_print_space f

let pp_oplist syn =
  Format.pp_print_list ~pp_sep:pp_comma
                       (pp_operand syn)

(*
 * Prefixes
 *)

type prefix =
  | PreLock

let prefix_string = function
  | PreLock -> "lock"

let pp_prefix f p =
  Format.pp_print_string f (prefix_string p);
  Format.pp_print_space f ()

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

type opcode =
  | X86OpJump of condition option
  | X86OpMov of size option
  | X86OpNop
  | X86OpDirective of string
  | X86OpUnknown of string

(** [make_att_suffixes f prefix] generates all of the various
    combinations of an opcode and its AT&T size suffixes. *)
let make_att_suffixes (f : size option -> opcode)
                      (prefix : string)
    : (opcode, string) List.Assoc.t =
  [ f None           , prefix
  ; f (Some X86SByte), prefix ^ "b"
  ; f (Some X86SWord), prefix ^ "w"
  ; f (Some X86SLong), prefix ^ "l"
  ]

module OpcodeTable =
  StringTable.Make
    (struct
      type t = opcode
      let table =
        let jumps =
          (X86OpJump None, "jmp") ::
            List.map ~f:(fun (x, s) -> X86OpJump (Some x), "j" ^ s)
                     ConditionTable.table
        in
        let movs = make_att_suffixes (fun x -> X86OpMov x) "mov" in
        let basics =
          [ X86OpNop, "nop"
          ]
        in
        List.concat [jumps; movs; basics]
    end)

let pp_opcode _ f =
  function
  | X86OpDirective s -> Format.fprintf f ".%s" s
  | X86OpUnknown s -> String.pp f s
  | opc ->
     opc
     |> OpcodeTable.to_string
     |> Option.value ~default:"<FIXME: OPCODE WITH NO STRING EQUIVALENT>"
     |> String.pp f

let%expect_test "pp_opcode: directive" =
  Format.printf "%a" (pp_opcode SynAtt) (X86OpDirective "text");
  Format.print_flush ();
  [%expect {| .text |}]

let%expect_test "pp_opcode: jmp" =
  Format.printf "%a" (pp_opcode SynAtt) (X86OpJump None);
  Format.print_flush ();
  [%expect {| jmp |}]

let%expect_test "pp_opcode: jge" =
  Format.printf "%a" (pp_opcode SynAtt) (X86OpJump (Some `GreaterEqual));
  Format.print_flush ();
  [%expect {| jge |}]

let%expect_test "pp_opcode: jnz" =
  Format.printf "%a" (pp_opcode SynAtt) (X86OpJump (Some (`Not `Zero)));
  Format.print_flush ();
  [%expect {| jnz |}]

let%expect_test "pp_opcode: mov" =
  Format.printf "%a" (pp_opcode SynAtt) (X86OpMov None);
  Format.print_flush ();
  [%expect {| mov |}]

let%expect_test "pp_opcode: movw" =
  Format.printf "%a" (pp_opcode SynAtt) (X86OpMov (Some X86SWord));
  Format.print_flush ();
  [%expect {| movw |}]

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

let pp_instruction syn f { prefix; opcode; operands } =
  Format.fprintf f
                 "@[@[%a%a@]@ %a@]"
                 (MyFormat.pp_option ~pp:pp_prefix) prefix
                 (pp_opcode syn) opcode
                 (pp_oplist syn) operands

(*
 * Statements
 *)

type statement =
  | StmInstruction of instruction
  | StmLabel of string
  | StmNop

let fold_map_statement_symbols ~f ~init =
  function
  | StmInstruction i ->
     Tuple.T2.map_snd ~f:(fun x -> StmInstruction x)
                      (fold_map_instruction_symbols ~f ~init i)
  | StmLabel l ->
     Tuple.T2.map_snd ~f:(fun x -> StmLabel x)
                      (f init l)
  | StmNop -> (init, StmNop)

let pp_statement syn f =
  function
  | StmInstruction i -> pp_instruction syn f i; Format.pp_print_cut f ()
  | StmLabel l -> Format.fprintf f "@[%s:@ @]" l
  | StmNop ->
     (* This blank space is deliberate, to make tabstops move across
        properly in litmus printing. *)
     Format.fprintf f " "; Format.pp_print_cut f ()

let pp_ast syn f ast =
  Format.pp_open_vbox f 0;
  (* We don't print newlines out here due to nops and labels. *)
  List.iter ~f:(pp_statement syn f) ast;
  Format.pp_close_box f ();
