(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   (parts (c) 2010-2018 Institut National
   de Recherche en Informatique et en Automatique, Jade Alglave, and Luc
   Maranget)

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

open Act_common
open Utils

(** [Has_dialect] is a signature for modules that report a specific dialect. *)
module type Has_dialect = sig
  (** [dialect] is the identifier of this module's associated x86 dialect. *)
  val dialect : Id.t
end

(** [S] is the interface of modules containing x86 dialect information. *)
module type S = sig
  include Has_dialect

  (** This lets us query a dialect's operand order. *)
  include Src_dst.S

  val has_size_suffix : bool
  (** [has_size_suffix] gets whether this dialect uses AT&T-style size
      suffixes. *)

  val symbolic_jump_type : [`Indirect | `Immediate]
  (** [symbolic_jump_type] gets the type of syntax this dialect _appears_ to
      use for symbolic jumps.

      In all x86 dialects, a jump to a label is `jCC LABEL`, where `CC` is
      `mp` or some condition. Because of the way we parse x86, the label
      resolves to different abstract syntax depending on the dialect.

      In AT&T, symbolic jumps look like indirect displacements; in Intel and
      Herd7, they look like immediate values. *)
end
