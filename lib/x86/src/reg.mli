(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   (parts (c) 2010-2018 Institut National de Recherche en Informatique et en
   Automatique, Jade Alglave, and Luc Maranget)

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE.

   This file derives in part from the Herd7 project
   (https://github.com/herd/herdtools7); its original attribution and
   copyright notice follow. *)

(* the diy toolsuite

   Jade Alglave, University College London, UK.

   Luc Maranget, INRIA Paris-Rocquencourt, France.

   Copyright 2010-present Institut National de Recherche en Informatique et
   en Automatique and the authors. All rights reserved.

   This software is governed by the CeCILL-B license under French law and by
   the rules of distribution of free software. You can use, and/ or
   redistribute the software under the terms of the CeCILL-B license as
   circulated by CEA, CNRS and INRIA at the following URL
   "http://www.cecill.info". We also give a copy in LICENSE.txt. *)

open Base

(** x86 registers *)

type gp8h = [`AH | `BH | `CH | `DH] [@@deriving equal, sexp]
(** [gp8h] enumerates the 8-bit 'high' general-purpose registers. *)

type gp8l = [`AL | `BL | `CL | `DL] [@@deriving enumerate, equal, sexp]
(** [gp8l] enumerates the 8-bit 'low' general-purpose registers. *)

type gp8 = [gp8h | gp8l] [@@deriving enumerate, equal, sexp]
(** [gp8] enumerates the 8-bit general-purpose registers. *)

type gp16 = [`AX | `BX | `CX | `DX] [@@deriving enumerate, equal, sexp]
(** [gp16] enumerates the 16-bit general-purpose registers. *)

type gp32 = [`EAX | `EBX | `ECX | `EDX] [@@deriving enumerate, equal, sexp]
(** [gp32] enumerates the 32-bit general-purpose registers. *)

type gp = [gp8 | gp16 | gp32] [@@deriving enumerate, equal, sexp]
(** [gp] enumerates the general-purpose registers. *)

type seg = [`CS | `DS | `SS | `ES | `FS | `GS]
[@@deriving enumerate, equal, sexp]
(** [seg] enumerates the segment registers. *)

type flag = [`CF | `PF | `AF | `ZF | `SF | `OF]
[@@deriving enumerate, equal, sexp]
(** [flag] enumerates the flag registers. *)

type sp16 = [seg | `BP | `SP | `SI | `DI]
[@@deriving enumerate, equal, sexp]
(** [sp16] enumerates the 16-bit special purpose registers. *)

type sp32 = [`EIP | `EBP | `ESP | `ESI | `EDI]
[@@deriving enumerate, equal, sexp]
(** [sp32] enumerates the 32-bit special-purpose registers. *)

type sp = [sp16 | sp32] [@@deriving enumerate, equal, sexp]
(** [sp] enumerates the special-purpose registers. *)

type reg8 = gp8
(** [reg8] enumerates all 8-bit registers. *)

type reg16 = [gp16 | sp16] [@@deriving enumerate, equal, sexp]
(** [reg16] enumerates all 16-bit registers. *)

type reg32 = [gp32 | sp32] [@@deriving enumerate, equal, sexp]
(** [reg32] enumerates all 32-bit registers. *)

type t = [reg8 | reg16 | reg32 | flag]
(** [t] enumerates all commonly used registers available in 32-bit x86. *)

include Act_utils.Enum.Extension_table with type t := t

include
  Act_abstract.Abstractable.S
    with type t := t
     and module Abs := Act_abstract.Register
