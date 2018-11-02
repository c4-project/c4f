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

open Utils
open Lib

(** [t] enumerates the various dialects of x86 syntax. *)
type t =
  | Att   (* AT&T syntax (like GNU as) *)
  | Intel (* Intel syntax *)
  | Herd7 (* Herd7 syntax (somewhere in between) *)
  [@@deriving sexp, compare, hash]

(** [STable] associates each dialect with its string name. *)
module STable : (StringTable.Intf with type t = t)

include Core.Identifiable.S_plain with type t := t

(** [pp f syn] pretty-prints a dialect name [syn] onto formatter
   [f]. *)
val pp : Format.formatter -> t -> unit

(** [Has_dialect] is a signature for modules that report a specific
    dialect. *)
module type Has_dialect = sig
  val dialect : t
end

(** [Intf] is the interface of modules containing x86 dialect information. *)
module type Intf = sig
  include Has_dialect

  (** This lets us query a dialect's operand order. *)
  include Src_dst.S

  (** [has_size_suffix] gets whether this dialect uses
   AT&T-style size suffixes. *)
  val has_size_suffix : bool

  (** [symbolic_jump_type] gets the type of syntax this dialect
     _appears_ to use for symbolic jumps.

      In all x86 dialects, a jump to a label is `jCC LABEL`, where
     `CC` is `mp` or some condition.  Because of the way we parse x86,
     the label resolves to different abstract syntax depending on the
     dialect.

      In AT&T, symbolic jumps look like indirect displacements; in
     Intel and Herd7, they look like immediate values. *)
  val symbolic_jump_type : [`Indirect | `Immediate ]
end

(** [ATT] describes the AT&T dialect of x86 assembly. *)
module ATT : Intf

(** [Intel] describes the Intel dialect of x86 assembly. *)
module Intel : Intf

(** [Herd7] describes the Herd7 dialect of x86 assembly. *)
module Herd7 : Intf
