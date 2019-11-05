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
module Au = Act_utils

type t = Unscaled of Reg.t | Scaled of Reg.t * int
[@@deriving sexp, variants, eq, compare]

(** Base mapper for indices *)
module Base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let map_m (x : t) ~unscaled ~scaled : t M.t =
    Variants.map x
      ~unscaled:(F.proc_variant1 unscaled)
      ~scaled:(F.proc_variant2 scaled)
end

(** Recursive mapper for registers *)
module On_registers :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Reg.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Reg

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module F = Travesty.Traversable.Helpers (M)

    let map_m t ~f =
      B.map_m t ~unscaled:f
        ~scaled:M.(fun (r, k) -> f r >>| fun r' -> (r', k))
  end
end)

module Q : Au.My_quickcheck.S_with_sexp with type t := t = struct
  let sexp_of_t = sexp_of_t

  module G = Base_quickcheck.Generator
  module O = Base_quickcheck.Observer
  module S = Base_quickcheck.Shrinker

  let anonymise = function
    | Unscaled reg ->
        `A reg
    | Scaled (reg, s) ->
        `B (reg, s)

  let deanonymise = function
    | `A reg ->
        Unscaled reg
    | `B (reg, s) ->
        Scaled (reg, s)

  let quickcheck_generator : t G.t =
    G.map ~f:deanonymise
      [%quickcheck.generator:
        [`A of Reg.t | `B of Reg.t * [%custom G.small_strictly_positive_int]]]

  let quickcheck_observer : t O.t =
    O.unmap ~f:anonymise
      [%quickcheck.observer: [`A of Reg.t | `B of Reg.t * int]]

  let quickcheck_shrinker : t S.t =
    S.map ~f:deanonymise ~f_inverse:anonymise
      [%quickcheck.shrinker: [`A of Reg.t | `B of Reg.t * int]]
end

include Q
