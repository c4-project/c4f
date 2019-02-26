(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

open Core_kernel
open Utils

module Type = Mini_type
module Constant = Ast_basic.Constant

type t =
  { ty    : Type.t
  ; value : Constant.t option
  }
[@@deriving sexp, make, eq, fields]
;;

module Quickcheck : Quickcheckable.S with type t := t = struct
  module G = Core_kernel.Quickcheck.Generator
  module O = Core_kernel.Quickcheck.Observer
  module S = Core_kernel.Quickcheck.Shrinker

  let to_tuple { ty; value } = ( ty, value )
  let of_tuple ( ty, value ) = { ty; value }

  let gen : t G.t =
    G.map (G.tuple2 Type.gen (Option.gen (Constant.gen)))
      ~f:of_tuple
  ;;

  let obs : t O.t =
    O.unmap (O.tuple2 Type.obs (Option.obs (Constant.obs)))
      ~f:to_tuple
  ;;

  let shrinker : t S.t =
    S.map (S.tuple2 Type.shrinker (Option.shrinker (Constant.shrinker)))
      ~f:of_tuple ~f_inverse:to_tuple
  ;;
end
include Quickcheck

module Named : Mini_intf.S_named with type elt := t = struct
  type nonrec t = t Mini_intf.named
  let equal : t -> t -> bool =
    Tuple2.equal ~eq1:C_identifier.equal ~eq2:equal
  ;;
end
