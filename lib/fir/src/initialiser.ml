(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = {ty: Type.t; value: Constant.t}
[@@deriving sexp, compare, equal, accessors, quickcheck]

let of_int ?(is_atomic : bool = false) ?(is_volatile : bool = false)
    (value : int) =
  {ty= Type.int ~is_atomic ~is_volatile (); value= Constant.int value}
