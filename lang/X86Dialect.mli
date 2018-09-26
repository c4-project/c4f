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

(** Enumeration of, and facts about, x86 dialects *)

open Core
open Utils

(** [t] enumerates the various dialects of x86 syntax. *)

type t =
  | Att   (* AT&T syntax (like GNU as) *)
  | Intel (* Intel syntax *)
  | Herd7 (* Herd7 syntax (somewhere in between) *)

(** [DialectMap] associates each dialect with its string name. *)
module Map : (StringTable.Intf with type t = t)

(** [sexp_of_t syn] converts [syn] into an S-expression. *)
val sexp_of_t : t -> Sexp.t
(** [t_of_sexp sexp] tries to interpret an S-expression [sexp] as
   a dialect name. *)
val t_of_sexp : Sexp.t -> t

(** [pp f syn] pretty-prints a dialect name [syn] onto formatter
   [f]. *)

val pp : Format.formatter -> t -> unit

(*
 * Traits
 *)

(** [operand_order] enumerates the different orders of operands for
   two-operand instructions. *)
type operand_order =
  | SrcDst (* Source then destination (AT&T) *)
  | DstSrc (* Destination then source (Intel, Litmus) *)

(** [operand_order_of dialect] gets [dialect]'s operand order.

Since the AST doesn't mark the order of operands itself, this function
   is necessary to make sense of two-operand instructions. *)
val operand_order_of : t -> operand_order
