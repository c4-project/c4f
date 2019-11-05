(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   (parts (c) 2010-2018 Institut National de Recherche en Informatique et en
   Automatique, Jade Alglave, and Luc Maranget)

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
module Au = Act_utils

module M = struct
  type gp8h = [`AH | `BH | `CH | `DH] [@@deriving enumerate, equal, sexp]

  type gp8l = [`AL | `BL | `CL | `DL] [@@deriving enumerate, equal, sexp]

  type gp8 = [gp8h | gp8l] [@@deriving enumerate, equal, sexp]

  type gp16 = [`AX | `BX | `CX | `DX] [@@deriving enumerate, equal, sexp]

  type gp32 = [`EAX | `EBX | `ECX | `EDX] [@@deriving enumerate, equal, sexp]

  type gp = [gp8 | gp16 | gp32] [@@deriving enumerate, equal, sexp]

  type seg = [`CS | `DS | `SS | `ES | `FS | `GS]
  [@@deriving enumerate, equal, sexp]

  type flag = [`CF | `PF | `AF | `ZF | `SF | `OF]
  [@@deriving enumerate, equal, sexp]

  type sp16 = [seg | `BP | `SP | `SI | `DI]
  [@@deriving enumerate, equal, sexp]

  type sp32 = [`EIP | `EBP | `ESP | `ESI | `EDI]
  [@@deriving enumerate, equal, sexp]

  type sp = [sp16 | sp32] [@@deriving enumerate, equal, sexp]

  type reg8 = gp8

  type reg16 = [gp16 | sp16] [@@deriving enumerate, equal, sexp]

  type reg32 = [gp32 | sp32] [@@deriving enumerate, equal, sexp]

  type t =
    [gp8 (* can't use reg8 here, it breaks sexp *) | reg16 | reg32 | flag]
  [@@deriving enumerate, equal, sexp]

  let table : (t, string) List.Assoc.t =
    [ (`AH, "AH")
    ; (`AL, "AL")
    ; (`AX, "AX")
    ; (`EAX, "EAX")
    ; (`BH, "BH")
    ; (`BL, "BL")
    ; (`BX, "BX")
    ; (`EBX, "EBX")
    ; (`CH, "CH")
    ; (`CL, "CL")
    ; (`CX, "CX")
    ; (`ECX, "ECX")
    ; (`DH, "DH")
    ; (`DL, "DL")
    ; (`DX, "DX")
    ; (`EDX, "EDX")
    ; (`BP, "BP")
    ; (`EBP, "EBP")
    ; (`SI, "SI")
    ; (`ESI, "ESI")
    ; (`DI, "DI")
    ; (`EDI, "EDI")
    ; (`SP, "SP")
    ; (`ESP, "ESP")
    ; (`CS, "CS")
    ; (`DS, "DS")
    ; (`SS, "SS")
    ; (`ES, "ES")
    ; (`FS, "FS")
    ; (`GS, "GS")
    ; (`CF, "CF")
    ; (`PF, "PF")
    ; (`AF, "AF")
    ; (`ZF, "ZF")
    ; (`SF, "SF")
    ; (`OF, "OF")
    ; (`EIP, "EIP") ]
end

include M

include Au.Enum.Extend_table (struct
  include M
  include Au.Enum.Make_from_enumerate (M)
end)

include Act_abstract.Abstractable.Make (struct
  type nonrec t = t

  module Abs = Act_abstract.Register

  let abstract : t -> Act_abstract.Register.t = function
    (* Technically, [E]SP is the 'stack pointer' on x86. However, stack
       offsets generally descend from [E]BP, so we map it to the 'abstract'
       stack pointer. *)
    | `BP | `EBP | `SP | `ESP ->
        Stack_pointer
    | #gp as reg ->
        General (to_string reg)
    | #sp | #flag ->
        Unknown
end)
