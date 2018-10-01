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

(** Pretty-printing for x86

This module is organised along dialect boundaries: first, we give an
   interface module [S] that exposes the most useful pretty-printers
   for the x86 AST; then, we give an implementation for each of the
   dialects we support.

Note that changing the pretty-printer from one dialect to another does
   *NOT* result in valid assembly output in that dialect!  The AST
   doesn't track things like which operand is source and which is
   destination, and these change between dialects.  *)

open X86Ast

module type S =
  sig
    val pp_reg : Format.formatter -> reg -> unit
    val pp_indirect : Format.formatter -> indirect -> unit
    val pp_location : Format.formatter -> location -> unit

    val pp_bop : Format.formatter -> bop -> unit
    val pp_operand : Format.formatter -> operand -> unit

    val pp_prefix : Format.formatter -> prefix -> unit

    (** [pp_opcode f op] pretty-prints opcode [op] on formatter [f]. *)
    val pp_opcode : Format.formatter -> opcode -> unit

    val pp_instruction : Format.formatter -> instruction -> unit

    val pp_statement : Format.formatter -> statement -> unit
end

(** [ATT] provides pretty-printing for AT&T-syntax x86. *)
module ATT : S
(** [Intel] provides pretty-printing for Intel-syntax x86. *)
module Intel : S
(** [Herd7] provides pretty-printing for Herd-syntax x86. *)
module Herd7 : S

val pp_ast : Format.formatter -> t -> unit

