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
open Base_quickcheck
module Tx = Travesty_base_exts

type t = Symbolic of string | Numeric of int
[@@deriving sexp, variants, equal, compare, quickcheck]

(* TODO(@MattWindsor91): generate valid symbols only? *)

(** Base mapper for displacements *)
module Base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let map_m (x : t) ~symbolic ~numeric : t M.t =
    Variants.map x
      ~symbolic:(F.proc_variant1 symbolic)
      ~numeric:(F.proc_variant1 numeric)
end

module On_symbols :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = string =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = String

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module F = Travesty.Traversable.Helpers (M)

    let map_m t ~f =
      B.map_m t
        ~symbolic:f (* Numeric displacements, of course, have no symbols *)
        ~numeric:M.return
  end
end)

include Act_abstract.Abstractable.Make (struct
  type nonrec t = t

  module Abs = Act_abstract.Address

  let abstract : t -> Act_abstract.Address.t = function
    | Numeric k ->
        Act_abstract.Address.Int k
    | Symbolic k ->
        Act_abstract.Address.Symbol k
end)

let as_symbol : t -> string option = On_symbols.find ~f:Tx.Fn.always
