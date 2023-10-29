(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(* Needed because Base shadows it: *)
module Ty = Type

open Base

type t = {ty: Ty.t; value: Constant.t}
[@@deriving sexp, compare, equal, accessors, quickcheck]

let of_int ?(is_atomic : bool = false) ?(is_volatile : bool = false)
    (value : int) =
  {ty= Ty.int ~is_atomic ~is_volatile (); value= Constant.int value}
