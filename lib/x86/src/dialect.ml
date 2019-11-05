(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor (parts (c) 2010-2018 Institut National
   de Recherche en Informatique et en Automatique, Jade Alglave, and Luc
   Maranget)

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.

   This file derives from the Herd7 project
   (https://github.com/herd/herdtools7); its original attribution and
   copyright notice follow. *)

(****************************************************************************)
(* the diy toolsuite *)
(*  *)
(* Jade Alglave, University College London, UK. *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France. *)
(*  *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved. *)
(*  *)
(* This software is governed by the CeCILL-B license under French law and *)
(* abiding by the rules of distribution of free software. You can use, *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt. *)
(****************************************************************************)

open Base
module A = Act_common
open Dialect_intf

module Make (B : Basic) : S = struct
  include B
  include A.Src_dst.Make (B)

  let make_jump_operand (jsym : string) : Ast.Operand.t =
    Ast.(
      let disp = Disp.Symbolic jsym in
      match symbolic_jump_type with
      | `Indirect ->
          Operand.Location (Location.Indirect (Indirect.make ~disp ()))
      | `Immediate ->
          Operand.Immediate disp)

  let call_to_symbol : string -> Ast.Instruction.t =
    Fn.compose Ast.Instruction.call make_jump_operand

  let jmp_to_symbol : string -> Ast.Instruction.t =
    Fn.compose Ast.Instruction.jmp make_jump_operand
end

(** Facts common to the AT&T-style dialects [Att] and [Gcc]. *)
module Att_like = struct
  let operand_order = A.Src_dst.Src_then_dst

  let symbolic_jump_type = `Indirect

  let has_size_suffix = true
end

module Att : S = Make (struct
  let dialect_id : A.Id.t = A.Id.of_string "att"

  let readme () : string =
    Act_utils.My_string.format_for_readme
      {|
        x86 dialect corresponding to the AT&T syntax natively understood
        by GNU as, and consequently GCC and similar compilers.
      |}

  include Att_like

  let is_asm_template = false
end)

module Gcc : S = Make (struct
  let dialect_id : A.Id.t = A.Id.of_string "gcc"

  let readme () : string =
    Act_utils.My_string.format_for_readme
      {|
        x86 dialect corresponding to the slightly escaped form of AT&T
        syntax used in GCC's asm templates.  This dialect mainly exists
        to make `act`'s job in emitting such templates easier, and isn't
        generally useful outside of that job.
      |}

  include Att_like

  let is_asm_template = true
end)

(** Facts common to the Intel-style dialects [Intel] and [herd7]. *)
module Intel_like = struct
  let operand_order = A.Src_dst.Dst_then_src

  let symbolic_jump_type = `Immediate

  let is_asm_template = false
end

module Intel : S = Make (struct
  let dialect_id : A.Id.t = A.Id.of_string "intel"

  let readme () : string =
    Act_utils.My_string.format_for_readme
      {|
        x86 dialect corresponding to the Intel syntax natively understood
        by Microsoft's build tools.
      |}

  include Intel_like

  let has_size_suffix = false
end)

module Herd7 : S = Make (struct
  let dialect_id : A.Id.t = A.Id.of_string "herd7"

  let readme () : string =
    Act_utils.My_string.format_for_readme
      {|
        x86 dialect corresponding to the syntax used in Herdtools7-style
        Litmus tests; generally Intel-style, but with some pieces of C
        and AT&T syntax carried over.
      |}

  include Intel_like

  (* Surprisingly, this is true---for some operations, anyway. *)
  let has_size_suffix = true
end)

let find_by_id (type a) (table : (A.Id.t, a) List.Assoc.t Lazy.t)
    ~(context : string) : (A.Id.t -> a Or_error.t) Staged.t =
  let id_type =
    Printf.sprintf "Unknown or unsupported x86 dialect (context: %s)" context
  in
  Staged.stage (fun id ->
      A.Id.try_find_assoc_with_suggestions (Lazy.force table) id ~id_type)
