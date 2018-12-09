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

open Base
open Ast

let disp_positive =
  function
  | None -> false
  | Some (Disp.Numeric k) -> 0 < k
  | _ -> true

module type Dialect = sig
  val pp_reg : Formatter.t -> Reg.t -> unit
  val pp_indirect : Formatter.t -> Indirect.t -> unit
  val pp_immediate : Formatter.t -> Disp.t -> unit
  val pp_comment
    :  pp:(Formatter.t -> 'a -> unit)
    -> Formatter.t
    -> 'a
    -> unit
end

module type Printer = sig
  include Dialect

  val pp_location : Formatter.t -> Location.t -> unit
  val pp_bop : Formatter.t -> Operand.bop -> unit
  val pp_operand : Formatter.t -> Operand.t -> unit
  val pp_prefix : Formatter.t -> prefix -> unit
  val pp_opcode : Formatter.t -> Opcode.t -> unit
  val pp_oplist : Formatter.t -> Operand.t list -> unit
  val pp_instruction : Formatter.t -> Instruction.t -> unit
  val pp_statement : Formatter.t -> Statement.t -> unit
end

(* Parts specific to all dialects *)
module Basic = struct
    (*
     * Displacements
     *)

  let pp_disp ?(show_zero = true) f = function
    | Disp.Symbolic s -> Fmt.string f s
    | Disp.Numeric  0 when not show_zero -> ()
    | Disp.Numeric  k -> Fmt.int f k
end

(* Parts specific to AT&T *)
module Att_specific = struct
  let pp_comment ~pp f = Fmt.pf f "@[<h># %a@]" pp

  let%expect_test "pp_comment: AT&T" =
    Fmt.pr "%a@."
      (pp_comment ~pp:String.pp) "AT&T comment";
    [%expect {| # AT&T comment |}]

  let pp_reg f reg =
    Fmt.pf f "@[%%%s@]" (Reg.to_string reg)

  let%expect_test "pp_reg: AT&T, ESP" =
    Fmt.pr "%a@." pp_reg `ESP;
    [%expect {| %ESP |}]

  let pp_index f = function
    | Index.Unscaled r -> pp_reg f r
    | Scaled (r, i) -> Fmt.pf f "%a,@ %d"
                         pp_reg r
                         i

  let pp_indirect f indirect =
    let pp_seg f = Fmt.pf f "%a:" pp_reg in

    let pp_bis f bo iso =
      match bo, iso with
      | None  , None -> ()
      | Some b, None ->
        Fmt.pf f "(%a)" pp_reg b
      | _     , Some i ->
        Fmt.(pf f "(%a,%a)" (option pp_reg)) bo
          pp_index i
    in

    let in_seg   = Indirect.seg   indirect in
    let in_base  = Indirect.base  indirect in
    let in_disp  = Indirect.disp  indirect in
    let in_index = Indirect.index indirect in

    Fmt.option pp_seg f in_seg;
    let show_zero = Option.(is_none in_base && is_none in_index) in
    Fmt.option (Basic.pp_disp ~show_zero) f in_disp;
    pp_bis f in_base in_index

  let%expect_test "pp_indirect: AT&T, +ve numeric displacement only" =
    Fmt.pr "%a@." pp_indirect
      (Indirect.make
         ~disp:(Disp.Numeric 2001)
         ());
    [%expect {| 2001 |}]

  let%expect_test "pp_indirect: AT&T, +ve disp and base" =
    Fmt.pr "%a@." pp_indirect
      (Indirect.make
         ~disp:(Disp.Numeric 76)
         ~base:`EAX
         ());
    [%expect {| 76(%EAX) |}]

  let%expect_test "pp_indirect: AT&T, zero disp only" =
    Fmt.pr "%a@." pp_indirect
      (Indirect.make
         ~disp:(Disp.Numeric 0)
         ());
    [%expect {| 0 |}]

  let%expect_test "pp_indirect: AT&T, -ve disp and base" =
    Fmt.pr "%a@." pp_indirect
      (Indirect.make
         ~disp:(Disp.Numeric (-42))
         ~base:`ECX
         ());
    [%expect {| -42(%ECX) |}]

  let%expect_test "pp_indirect: AT&T, base only" =
    Fmt.pr "%a@." pp_indirect
      (Indirect.make
         ~base:`EDX
         ());
    [%expect {| (%EDX) |}]

  let%expect_test "pp_indirect: AT&T, zero disp and base" =
    Fmt.pr "%a@." pp_indirect
      (Indirect.make
         ~disp:(Disp.Numeric 0)
         ~base:`EDX
         ());
    [%expect {| (%EDX) |}]

  let pp_immediate f = Fmt.pf f "@[$%a@]"
      (Basic.pp_disp ~show_zero:true)

  let%expect_test "pp_immediate: AT&T, positive number" =
    Fmt.pr "%a@." pp_immediate (Disp.Numeric 24);
    [%expect {| $24 |}]

  let%expect_test "pp_immediate: AT&T, zero" =
    Fmt.pr "%a@." pp_immediate (Disp.Numeric 0);
    [%expect {| $0 |}]

  let%expect_test "pp_immediate: AT&T, negative number" =
    Fmt.pr "%a@." pp_immediate (Disp.Numeric (-42));
    [%expect {| $-42 |}]


  let%expect_test "pp_immediate: AT&T, symbolic" =
    Fmt.pr "%a@." pp_immediate (Disp.Symbolic "kappa");
    [%expect {| $kappa |}]
end

(** Parts specific to Intel *)
module Intel_specific = struct
  let pp_comment ~pp f = Fmt.pf f "@[<h>; %a@]" pp

  let%expect_test "pp_comment: Intel" =
    Fmt.pr "%a@."
      (pp_comment ~pp:String.pp) "intel comment";
    [%expect {| ; intel comment |}]
end

(** Parts specific to Herd7 *)
module Herd7_specific = struct
  let pp_comment ~pp f = Fmt.pf f "@[<h>// %a@]" pp

  let%expect_test "pp_comment: Herd7" =
    Fmt.pr "%a@."
      (pp_comment ~pp:String.pp) "herd comment";
    [%expect {| // herd comment |}]
end

(** Parts common to Intel and Herd7 *)
module Intel_and_herd7 = struct
    let pp_reg f reg = String.pp f (Reg.to_string reg)

    let%expect_test "pp_reg: intel, EAX" =
      Fmt.pr "%a@." pp_reg `EAX;
      [%expect {| EAX |}]

    let pp_index f = function
      | Index.Unscaled r -> pp_reg f r
      | Scaled (r, i) -> Fmt.pf f "%a*%d" pp_reg r i

    let pp_seg f = Fmt.pf f "%a:" pp_reg

    let pp_indirect =
      Fmt.(
        box (
          brackets (
            fun f indirect ->
              let in_seg   = Indirect.seg   indirect in
              let in_base  = Indirect.base  indirect in
              let in_disp  = Indirect.disp  indirect in
              let in_index = Indirect.index indirect in

              (* seg:base+index*scale+disp *)

              option pp_seg f in_seg;
              option pp_reg f in_base;

              let plus_between_b_i =
                Option.(is_some in_base && is_some in_index)
              in if plus_between_b_i then char f '+';

              option pp_index f in_index;

              let plus_between_bis_d =
                Option.(is_some in_base || is_some in_index)
                && disp_positive in_disp
              in if plus_between_bis_d then char f '+';

              let show_zero =
                Option.(is_none in_base && is_none in_index)
              in option (Basic.pp_disp ~show_zero) f in_disp
          )
        )
      )
    ;;

    let%expect_test "pp_indirect: intel, +ve numeric displacement only" =
      Fmt.pr "%a@." pp_indirect
        (Indirect.make ~disp:(Disp.Numeric 2001) ());
      [%expect {| [2001] |}]

    let%expect_test "pp_indirect: Intel, +ve disp and base" =
      Fmt.pr "%a@." pp_indirect
        (Indirect.make
           ~disp:(Disp.Numeric 76)
           ~base:`EAX
           ());
      [%expect {| [EAX+76] |}]


    let%expect_test "pp_indirect: Intel, zero disp only" =
      Fmt.pr "%a@." pp_indirect
        (Indirect.make ~disp:(Disp.Numeric 0) ());
      [%expect {| [0] |}]


    let%expect_test "pp_indirect: Intel, +ve disp and base" =
      Fmt.pr "%a@." pp_indirect
        (Indirect.make
           ~disp:(Disp.Numeric (-42))
           ~base:`ECX
           ());
      [%expect {| [ECX-42] |}]

    let%expect_test "pp_indirect: Intel, base only" =
      Fmt.pr "%a@." pp_indirect
        (Indirect.make
           ~base:`EDX
           ());
      [%expect {| [EDX] |}]

    let%expect_test "pp_indirect: Intel, zero disp and base" =
      Fmt.pr "%a@." pp_indirect
        (Indirect.make
           ~disp:(Disp.Numeric 0)
           ~base:`EDX
           ());
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
      | Operand.BopPlus -> Fmt.char f '+'
      | BopMinus -> Fmt.char f '-'

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
      | Location.Indirect i -> pp_indirect f i
      | Reg r -> pp_reg f r

    let rec pp_operand f = function
      | Operand.Location l -> pp_location f l;
      | Operand.Immediate d -> pp_immediate f d;
      | Operand.String s ->
         Fmt.pf f "\"%s\"" (Staged.unstage string_escape s)
      | Operand.Typ ty ->
         Fmt.pf f "@@%s" ty
      | Operand.Bop (l, b, r) ->
        Fmt.box pp_bop_operand f (l, b, r)
    and pp_bop_operand f (l, b, r) =
      pp_operand f l;
      pp_bop f b;
      pp_operand f r
    ;;

    let pp_oplist f = function
      | [] -> ()
      | operands ->
        (* Glue between operator and operands *)
        Fmt.(prefix sp (list ~sep:comma pp_operand)) f operands
    ;;

    (*
     * Prefixes
     *)

    let prefix_string = function
      | PreLock -> "lock"

    let pp_prefix = Fmt.(suffix sp (using prefix_string string))

    (*
     * Opcodes
     *)

    let pp_opcode f = function
      | Opcode.Directive s -> Fmt.pf f ".%s" s
      | Unknown s -> String.pp f s
      | Basic opc ->
         opc
         |> Opcode.Basic.to_string
         |> Option.value ~default:"<FIXME: OPCODE WITH NO STRING EQUIVALENT>"
         |> String.pp f
      | Jump opc ->
         opc
         |> Opcode.Jump.to_string
         |> Option.value ~default:"<FIXME: JUMP WITH NO STRING EQUIVALENT>"
         |> String.pp f
      | Sized opc ->
         (* TODO: Intel syntax *)
        opc
         |> Opcode.Sized.to_string
         |> Option.value ~default:"<FIXME: JUMP WITH NO STRING EQUIVALENT>"
         |> String.pp f

    (*
     * Instructions
     *)

    let pp_instruction f { Instruction.prefix = p; opcode; operands } =
      Fmt.(
        pf f "@[@[%a%a@]%a@]"
          (option pp_prefix) p
          pp_opcode opcode
          pp_oplist operands
      )

    (*
     * Statements
     *)

    let pp_statement f = function
      | Statement.Instruction i -> pp_instruction f i; Fmt.cut f ()
      | Label l -> Fmt.pf f "@[%s:@ @]" l
      | Nop ->
         (* This blank space is deliberate, to make tabstops move across
        properly in litmus printing. *)
         Fmt.pf f " "; Fmt.cut f ()
  end

module Att = Make (Att_specific)

let%expect_test "pp_opcode: directive" =
  Fmt.pr "%a@." Att.pp_opcode (Opcode.Directive "text");
  [%expect {| .text |}]

let%expect_test "pp_opcode: jmp" =
  Fmt.pr "%a@." Att.pp_opcode (Opcode.Jump `Unconditional);
  [%expect {| jmp |}]

let%expect_test "pp_opcode: jge" =
  Fmt.pr "%a@." Att.pp_opcode (Opcode.Jump (`Conditional `GreaterEqual));
  [%expect {| jge |}]

let%expect_test "pp_opcode: jnz" =
  Fmt.pr "%a@." Att.pp_opcode (Opcode.Jump (`Conditional (`Not `Zero)));
  [%expect {| jnz |}]

let%expect_test "pp_opcode: mov" =
  Fmt.pr "%a@." Att.pp_opcode (Opcode.Basic `Mov);
  [%expect {| mov |}]

let%expect_test "pp_opcode: movw (AT&T)" =
  Fmt.pr "%a@." Att.pp_opcode (Opcode.Sized (`Mov, Opcode.Size.Word));
  [%expect {| movw |}]

module Intel = Make (struct
    include Intel_specific
    include Intel_and_herd7
  end)
module Herd7 = Make (struct
    include Herd7_specific
    include Intel_and_herd7
  end)

let pp_ast =
  Fmt.box (fun f ast ->
      let pps =
        match ast.syntax with
        | Dialect.Att -> Att.pp_statement
        | Dialect.Intel -> Intel.pp_statement
        | Dialect.Herd7 -> Herd7.pp_statement
      in
      (* We don't print newlines out here due to nops and labels. *)
      List.iter ~f:(pps f) ast.program
    )
;;
