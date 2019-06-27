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

(** x86 AST: indirect memory accesses. *)

open Base

type t [@@deriving sexp, compare, equal, quickcheck]
(** [t] is the opaque type of indirect memory accesses. *)

val make :
  ?seg:Reg.t -> ?disp:Disp.t -> ?base:Reg.t -> ?index:Index.t -> unit -> t
(** [make ?seg ?disp ?base ?index ()] makes an [Indirect] with the given
    fields (if present). *)

(** {2 Projections} *)

val base : t -> Reg.t option
(** [base] gets the indirect base, if any. *)

val seg : t -> Reg.t option
(** [seg] gets the indirect segment, if any. *)

val disp : t -> Disp.t option
(** [disp] gets the indirect displacement, if any. *)

val index : t -> Index.t option
(** [index] gets the indirect index, if any. *)

val as_disp_only : t -> Disp.t option
(** [as_disp_only i] returns [Some d] if [i] contains only a displacement
    [d], and [None] otherwise. *)

val as_symbolic_disp_only : t -> string option
(** [as_symbolic_disp_only i] returns [Some s] if [i] contains only a
    symbolic displacement [s], and [None] otherwise. *)

(** {2 Traversals} *)

(** [On_registers] permits enumerating and folding over registers inside a
    memory access. *)
module On_registers :
  Travesty.Traversable_types.S0 with type t := t and type Elt.t = Reg.t

(** [On_symbols] permits enumerating and folding over symbols inside a
    memory access. *)
module On_symbols :
  Travesty.Traversable_types.S0 with type t := t and type Elt.t = string
