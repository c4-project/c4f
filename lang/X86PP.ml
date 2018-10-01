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

let disp_positive =
  function
  | None -> false
  | Some (DispNumeric k) -> 0 < k
  | _ -> true

module type Dialect =
  sig
    val pp_reg : Format.formatter -> reg -> unit
    val pp_indirect : Format.formatter -> indirect -> unit
    val pp_immediate : Format.formatter -> disp -> unit
  end

module type S =
  sig
    val pp_reg : Format.formatter -> reg -> unit
    val pp_indirect : Format.formatter -> indirect -> unit
    val pp_location : Format.formatter -> location -> unit
    val pp_bop : Format.formatter -> bop -> unit
    val pp_operand : Format.formatter -> operand -> unit
    val pp_prefix : Format.formatter -> prefix -> unit
    val pp_opcode : Format.formatter -> opcode -> unit
    val pp_instruction : Format.formatter -> instruction -> unit
    val pp_statement : Format.formatter -> statement -> unit
  end

module Basic =
  struct
    (*
     * Displacements
     *)

    let pp_disp ?(show_zero = true) f =
      function
      | DispSymbolic s -> Format.pp_print_string f s
      | DispNumeric  0 when not show_zero -> ()
      | DispNumeric  k -> Format.pp_print_int    f k
  end

module ATTSpecific =
  struct
    let pp_reg f reg =
      Format.fprintf f "@[%%%s@]" (RegTable.to_string_exn reg)

    let%expect_test "pp_reg: AT&T, ESP" =
      Format.printf "%a@." pp_reg ESP;
      [%expect {| %ESP |}]

    let pp_index f =
      function
      | Unscaled r -> pp_reg f r
      | Scaled (r, i) -> Format.fprintf f "%a,@ %d"
                                        pp_reg r
                                        i

    let pp_indirect f {in_seg; in_disp; in_base; in_index} =
      let pp_seg f = Format.fprintf f "%a:" pp_reg in

      let pp_bis f bo iso =
        match bo, iso with
        | None  , None -> ()
        | Some b, None ->
           Format.fprintf f "(%a)"
                          pp_reg b
        | _     , Some i ->
           Format.fprintf f "(%a,%a)"
                          (MyFormat.pp_option ~pp:pp_reg) bo
                          pp_index i
      in

      MyFormat.pp_option f ~pp:pp_seg in_seg;
      let show_zero = in_base = None && in_index = None in
      MyFormat.pp_option f ~pp:(Basic.pp_disp ~show_zero) in_disp;
      pp_bis f in_base in_index

    let%expect_test "pp_indirect: AT&T, +ve numeric displacement only" =
      Format.printf "%a@." pp_indirect
                    { in_seg = None
                    ; in_disp = Some (DispNumeric 2001)
                    ; in_base = None
                    ; in_index = None
                    };
      [%expect {| 2001 |}]

    let%expect_test "pp_indirect: AT&T, +ve disp and base" =
      Format.printf "%a@." pp_indirect
                    { in_seg = None
                    ; in_disp = Some (DispNumeric 76)
                    ; in_base = Some EAX
                    ; in_index = None
                    };
      [%expect {| 76(%EAX) |}]

    let%expect_test "pp_indirect: AT&T, zero disp only" =
      Format.printf "%a@." pp_indirect
                    { in_seg = None
                    ; in_disp = Some (DispNumeric 0)
                    ; in_base = None
                    ; in_index = None
                    };
      [%expect {| 0 |}]

    let%expect_test "pp_indirect: AT&T, -ve disp and base" =
      Format.printf "%a@." pp_indirect
                    { in_seg = None
                    ; in_disp = Some (DispNumeric (-42))
                    ; in_base = Some ECX
                    ; in_index = None
                    };
      [%expect {| -42(%ECX) |}]

    let%expect_test "pp_indirect: AT&T, base only" =
      Format.printf "%a@." pp_indirect
                    { in_seg = None
                    ; in_disp = None
                    ; in_base = Some EDX
                    ; in_index = None
                    };
      [%expect {| (%EDX) |}]

    let%expect_test "pp_indirect: AT&T, zero disp and base" =
      Format.printf "%a@." pp_indirect
                    { in_seg = None
                    ; in_disp = Some (DispNumeric 0)
                    ; in_base = Some EDX
                    ; in_index = None
                    };
      [%expect {| (%EDX) |}]

    let pp_immediate f = Format.fprintf f "@[$%a@]"
                                        (Basic.pp_disp ~show_zero:true)

    let%expect_test "pp_immediate: AT&T, positive number" =
      Format.printf "%a@." pp_immediate (DispNumeric 24);
      [%expect {| $24 |}]

    let%expect_test "pp_immediate: AT&T, zero" =
      Format.printf "%a@." pp_immediate (DispNumeric 0);
      [%expect {| $0 |}]

    let%expect_test "pp_immediate: AT&T, negative number" =
      Format.printf "%a@." pp_immediate (DispNumeric (-42));
      [%expect {| $-42 |}]


    let%expect_test "pp_immediate: AT&T, symbolic" =
      Format.printf "%a@." pp_immediate (DispSymbolic "kappa");
      [%expect {| $kappa |}]
  end

module IntelSpecific =
  struct
    let pp_reg f reg = String.pp f (RegTable.to_string_exn reg)

    let%expect_test "pp_reg: intel, EAX" =
      Format.printf "%a@." pp_reg EAX;
      [%expect {| EAX |}]

    let pp_index f =
      function
      | Unscaled r -> pp_reg f r
      | Scaled (r, i) -> Format.fprintf f "%a*%d"
                                        pp_reg r
                                        i

    let pp_indirect f {in_seg; in_disp; in_base; in_index} =
      let pp_seg f = Format.fprintf f "%a:" pp_reg in

      Format.pp_open_box f 0;
      Format.pp_print_char f '[';

      (* seg:base+index*scale+disp *)

      MyFormat.pp_option f ~pp:pp_seg in_seg;

      MyFormat.pp_option f ~pp:pp_reg in_base;

      let plus_between_b_i = in_base <> None && in_index <> None in
      if plus_between_b_i then Format.pp_print_char f '+';

      MyFormat.pp_option f ~pp:pp_index in_index;

      let plus_between_bis_d =
        (in_base <> None || in_index <> None)
        && disp_positive in_disp
      in
      if plus_between_bis_d then Format.pp_print_char f '+';

      let show_zero = in_base = None && in_index = None in
      MyFormat.pp_option f ~pp:(Basic.pp_disp ~show_zero) in_disp;

      Format.pp_print_char f ']';
      Format.pp_close_box f ()

    let%expect_test "pp_indirect: intel, +ve numeric displacement only" =
      Format.printf "%a@." pp_indirect
                    { in_seg = None
                    ; in_disp = Some (DispNumeric 2001)
                    ; in_base = None
                    ; in_index = None
                    };
      [%expect {| [2001] |}]

    let%expect_test "pp_indirect: Intel, +ve disp and base" =
      Format.printf "%a@." pp_indirect
                    { in_seg = None
                    ; in_disp = Some (DispNumeric 76)
                    ; in_base = Some EAX
                    ; in_index = None
                    };
      [%expect {| [EAX+76] |}]


    let%expect_test "pp_indirect: Intel, zero disp only" =
      Format.printf "%a@." pp_indirect
                    { in_seg = None
                    ; in_disp = Some (DispNumeric 0)
                    ; in_base = None
                    ; in_index = None
                    };
      [%expect {| [0] |}]


    let%expect_test "pp_indirect: Intel, +ve disp and base" =
      Format.printf "%a@." pp_indirect
                    { in_seg = None
                    ; in_disp = Some (DispNumeric (-42))
                    ; in_base = Some ECX
                    ; in_index = None
                    };
      [%expect {| [ECX-42] |}]

    let%expect_test "pp_indirect: Intel, base only" =
      Format.printf "%a@." pp_indirect
                    { in_seg = None
                    ; in_disp = None
                    ; in_base = Some EDX
                    ; in_index = None
                    };
      [%expect {| [EDX] |}]

    let%expect_test "pp_indirect: Intel, zero disp and base" =
      Format.printf "%a@." pp_indirect
                    { in_seg = None
                    ; in_disp = Some (DispNumeric 0)
                    ; in_base = Some EDX
                    ; in_index = None
                    };
      [%expect {| [EDX] |}]

    let pp_immediate = (Basic.pp_disp ~show_zero:true)
  end

module Make (D : Dialect) =
  struct
    include Basic
    include D

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

    let pp_location f =
      function
      | LocIndirect i -> pp_indirect f i
      | LocReg r -> pp_reg f r

    let rec pp_operand f =
      function
      | OperandLocation l -> pp_location f l;
      | OperandImmediate d -> pp_immediate f d;
      | OperandString s ->
         Format.fprintf f "\"%s\"" (Staged.unstage string_escape s)
      | OperandBop (l, b, r) ->
         Format.pp_open_box f 0;
         pp_operand f l;
         pp_bop f b;
         pp_operand f r;
         Format.pp_close_box f ()

    let pp_comma f =
      Format.pp_print_char f ',';
      Format.pp_print_space f

    let pp_oplist =
      Format.pp_print_list ~pp_sep:pp_comma
                           pp_operand

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

    let pp_opcode f =
      function
      | OpDirective s -> Format.fprintf f ".%s" s
      | OpUnknown s -> String.pp f s
      | OpBasic opc ->
         opc
         |> BasicOpcodeTable.to_string
         |> Option.value ~default:"<FIXME: OPCODE WITH NO STRING EQUIVALENT>"
         |> String.pp f
      | OpJump opc ->
         opc
         |> JumpTable.to_string
         |> Option.value ~default:"<FIXME: JUMP WITH NO STRING EQUIVALENT>"
         |> String.pp f
      | OpSized (opc, sz) ->
         (* TODO: Intel syntax *)
         (opc, sz)
         |> ATTSizedOpcodeTable.to_string
         |> Option.value ~default:"<FIXME: JUMP WITH NO STRING EQUIVALENT>"
         |> String.pp f

    (*
     * Instructions
     *)

    let pp_instruction f { prefix; opcode; operands } =
      Format.fprintf f
                     "@[@[%a%a@]@ %a@]"
                     (MyFormat.pp_option ~pp:pp_prefix) prefix
                     pp_opcode opcode
                     pp_oplist operands

    (*
     * Statements
     *)

    let pp_statement f =
      function
      | StmInstruction i -> pp_instruction f i; Format.pp_print_cut f ()
      | StmLabel l -> Format.fprintf f "@[%s:@ @]" l
      | StmNop ->
         (* This blank space is deliberate, to make tabstops move across
        properly in litmus printing. *)
         Format.fprintf f " "; Format.pp_print_cut f ()
  end

module ATT = Make(ATTSpecific)

let%expect_test "pp_opcode: directive" =
  Format.printf "%a@." ATT.pp_opcode (OpDirective "text");
  [%expect {| .text |}]

let%expect_test "pp_opcode: jmp" =
  Format.printf "%a@." ATT.pp_opcode (OpJump None);
  [%expect {| jmp |}]

let%expect_test "pp_opcode: jge" =
  Format.printf "%a@." ATT.pp_opcode (OpJump (Some `GreaterEqual));
  [%expect {| jge |}]

let%expect_test "pp_opcode: jnz" =
  Format.printf "%a@." ATT.pp_opcode (OpJump (Some (`Not `Zero)));
  [%expect {| jnz |}]

let%expect_test "pp_opcode: mov" =
  Format.printf "%a@." ATT.pp_opcode (OpBasic `Mov);
  [%expect {| mov |}]

let%expect_test "pp_opcode: movw (AT&T)" =
  Format.printf "%a@." ATT.pp_opcode (OpSized (`Mov, X86SWord));
  [%expect {| movw |}]

module Intel = Make(IntelSpecific)
module Herd7 = Make(IntelSpecific) (* FIXME! *)

let pp_ast f ast =
  Format.pp_open_vbox f 0;
  let pps =
    match ast.syntax with
    | X86Dialect.Att -> ATT.pp_statement
    | X86Dialect.Intel -> Intel.pp_statement
    | X86Dialect.Herd7 -> Herd7.pp_statement
  in
  (* We don't print newlines out here due to nops and labels. *)
  List.iter ~f:(pps f) ast.program;
  Format.pp_close_box f ();
