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
open X86Ast

let pp_reg syn f reg =
  Format.pp_open_box f 0;
  if syn = X86Dialect.Att then Format.pp_print_char f '%';
  Format.pp_print_string f (RegTable.to_string_exn reg);
  Format.pp_close_box f ()

let%expect_test "pp_opcode: AT&T, ESP" =
  Format.printf "%a@." (pp_reg X86Dialect.Att) ESP;
  [%expect {| %ESP |}]

let%expect_test "pp_opcode: intel, EAX" =
  Format.printf "%a@." (pp_reg X86Dialect.Intel) EAX;
  [%expect {| EAX |}]

(*
 * Displacements
 *)

let pp_disp ?(show_zero = true) f =
  function
  | DispSymbolic s -> Format.pp_print_string f s
  | DispNumeric  0 when not show_zero -> ()
  | DispNumeric  k -> Format.pp_print_int    f k

(*
 * Indices
 *)

let pp_index syn f =
  function
  | Unscaled r -> pp_reg syn f r
  | Scaled (r, i) -> Format.fprintf f
                                    "%a%s%d"
                                    (pp_reg syn) r
                                    (match syn with
                                     | X86Dialect.Att -> ","
                                     | X86Dialect.Intel -> "*")
                                    i
(*
 * Memory addresses
 *)

let pp_seg syn f =
  Format.fprintf f "%a:" (pp_reg syn)

let pp_bis_att f bo iso =
  match bo, iso with
  | None  , None -> ()
  | Some b, None ->
     Format.fprintf f "(%a)"
                    (pp_reg X86Dialect.Att) b
  | _     , Some i ->
     Format.fprintf f "(%a,%a)"
                    (MyFormat.pp_option ~pp:(pp_reg X86Dialect.Att)) bo
                    (pp_index X86Dialect.Att) i

let pp_indirect_att f {in_seg; in_disp; in_base; in_index} =
  MyFormat.pp_option f ~pp:(pp_seg X86Dialect.Att) in_seg;
  let show_zero = in_base = None && in_index = None in
  MyFormat.pp_option f ~pp:(pp_disp ~show_zero) in_disp;
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

  MyFormat.pp_option f ~pp:(pp_seg X86Dialect.Intel) in_seg;

  MyFormat.pp_option f ~pp:(pp_reg X86Dialect.Intel) in_base;

  let plus_between_b_i = in_base <> None && in_index <> None in
  if plus_between_b_i then Format.pp_print_char f '+';

  MyFormat.pp_option f ~pp:(pp_index X86Dialect.Intel) in_index;

  let plus_between_bis_d =
    (in_base <> None || in_index <> None)
    && disp_positive in_disp
  in
  if plus_between_bis_d then Format.pp_print_char f '+';

  let show_zero = in_base = None && in_index = None in
  MyFormat.pp_option f ~pp:(pp_disp ~show_zero) in_disp;

  Format.pp_print_char f ']';
  Format.pp_close_box f ()

let pp_indirect syn f ind =
  Format.pp_open_box f 0;
  if syn = X86Dialect.Att then pp_indirect_att f ind else pp_indirect_intel f ind;
  Format.pp_close_box f ()

let%expect_test "pp_indirect: intel, +ve numeric displacement only" =
  Format.printf "%a@." (pp_indirect X86Dialect.Intel)
                { in_seg = None
                ; in_disp = Some (DispNumeric 2001)
                ; in_base = None
                ; in_index = None
                };
  [%expect {| [2001] |}]

let%expect_test "pp_indirect: AT&T, +ve numeric displacement only" =
  Format.printf "%a@." (pp_indirect X86Dialect.Att)
                { in_seg = None
                ; in_disp = Some (DispNumeric 2001)
                ; in_base = None
                ; in_index = None
                };
  [%expect {| 2001 |}]

let%expect_test "pp_indirect: Intel, +ve disp and base" =
  Format.printf "%a@." (pp_indirect X86Dialect.Intel)
                { in_seg = None
                ; in_disp = Some (DispNumeric 76)
                ; in_base = Some EAX
                ; in_index = None
                };
  [%expect {| [EAX+76] |}]

let%expect_test "pp_indirect: AT&T, +ve disp and base" =
  Format.printf "%a@." (pp_indirect X86Dialect.Att)
                { in_seg = None
                ; in_disp = Some (DispNumeric 76)
                ; in_base = Some EAX
                ; in_index = None
                };
  [%expect {| 76(%EAX) |}]

let%expect_test "pp_indirect: Intel, zero disp only" =
  Format.printf "%a@." (pp_indirect X86Dialect.Intel)
                { in_seg = None
                ; in_disp = Some (DispNumeric 0)
                ; in_base = None
                ; in_index = None
                };
  [%expect {| [0] |}]


let%expect_test "pp_indirect: AT&T, zero disp only" =
  Format.printf "%a@." (pp_indirect X86Dialect.Att)
                { in_seg = None
                ; in_disp = Some (DispNumeric 0)
                ; in_base = None
                ; in_index = None
                };
  [%expect {| 0 |}]

let%expect_test "pp_indirect: Intel, +ve disp and base" =
  Format.printf "%a@." (pp_indirect X86Dialect.Intel)
                { in_seg = None
                ; in_disp = Some (DispNumeric (-42))
                ; in_base = Some ECX
                ; in_index = None
                };
  [%expect {| [ECX-42] |}]

let%expect_test "pp_indirect: AT&T, -ve disp and base" =
  Format.printf "%a@." (pp_indirect X86Dialect.Att)
                { in_seg = None
                ; in_disp = Some (DispNumeric (-42))
                ; in_base = Some ECX
                ; in_index = None
                };
  [%expect {| -42(%ECX) |}]

let%expect_test "pp_indirect: Intel, base only" =
  Format.printf "%a@." (pp_indirect X86Dialect.Intel)
                { in_seg = None
                ; in_disp = None
                ; in_base = Some EDX
                ; in_index = None
                };
  [%expect {| [EDX] |}]

let%expect_test "pp_indirect: AT&T, base only" =
  Format.printf "%a@." (pp_indirect X86Dialect.Att)
                { in_seg = None
                ; in_disp = None
                ; in_base = Some EDX
                ; in_index = None
                };
  [%expect {| (%EDX) |}]

let%expect_test "pp_indirect: Intel, zero disp and base" =
  Format.printf "%a@." (pp_indirect X86Dialect.Intel)
                { in_seg = None
                ; in_disp = Some (DispNumeric 0)
                ; in_base = Some EDX
                ; in_index = None
                };
  [%expect {| [EDX] |}]

let%expect_test "pp_indirect: AT&T, zero disp and base" =
  Format.printf "%a@." (pp_indirect X86Dialect.Att)
                { in_seg = None
                ; in_disp = Some (DispNumeric 0)
                ; in_base = Some EDX
                ; in_index = None
                };
  [%expect {| (%EDX) |}]

(*
 * Operators
 *)

let pp_bop f = function
  | BopPlus -> Format.pp_print_char f '+'
  | BopMinus -> Format.pp_print_char f '-'

(*
 * Operands
 *)

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
     if syn = X86Dialect.Att then Format.pp_print_char f '$';
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

let prefix_string = function
  | PreLock -> "lock"

let pp_prefix f p =
  Format.pp_print_string f (prefix_string p);
  Format.pp_print_space f ()


(*
 * Opcodes
 *)

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
  Format.printf "%a@." (pp_opcode X86Dialect.Att) (X86OpDirective "text");
  [%expect {| .text |}]

let%expect_test "pp_opcode: jmp" =
  Format.printf "%a@." (pp_opcode X86Dialect.Att) (X86OpJump None);
  [%expect {| jmp |}]

let%expect_test "pp_opcode: jge" =
  Format.printf "%a@." (pp_opcode X86Dialect.Att) (X86OpJump (Some `GreaterEqual));
  [%expect {| jge |}]

let%expect_test "pp_opcode: jnz" =
  Format.printf "%a@." (pp_opcode X86Dialect.Att) (X86OpJump (Some (`Not `Zero)));
  [%expect {| jnz |}]

let%expect_test "pp_opcode: mov" =
  Format.printf "%a@." (pp_opcode X86Dialect.Att) (X86OpMov None);
  [%expect {| mov |}]

let%expect_test "pp_opcode: movw" =
  Format.printf "%a@." (pp_opcode X86Dialect.Att) (X86OpMov (Some X86SWord));
  [%expect {| movw |}]

(*
 * Instructions
 *)

let pp_instruction syn f { prefix; opcode; operands } =
  Format.fprintf f
                 "@[@[%a%a@]@ %a@]"
                 (MyFormat.pp_option ~pp:pp_prefix) prefix
                 (pp_opcode syn) opcode
                 (pp_oplist syn) operands

(*
 * Statements
 *)

let pp_statement syn f =
  function
  | StmInstruction i -> pp_instruction syn f i; Format.pp_print_cut f ()
  | StmLabel l -> Format.fprintf f "@[%s:@ @]" l
  | StmNop ->
     (* This blank space is deliberate, to make tabstops move across
        properly in litmus printing. *)
     Format.fprintf f " "; Format.pp_print_cut f ()

let pp_ast f ast =
  Format.pp_open_vbox f 0;
  (* We don't print newlines out here due to nops and labels. *)
  List.iter ~f:(pp_statement ast.syntax f) ast.program;
  Format.pp_close_box f ();
