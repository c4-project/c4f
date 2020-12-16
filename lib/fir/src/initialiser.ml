(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = {ty: Type.t; value: Constant.t}
[@@deriving sexp, compare, equal, accessors, quickcheck]

let of_int ?(is_atomic : bool = false) ?(is_volatile : bool = false)
    (value : int) =
  {ty= Type.int ~is_atomic ~is_volatile (); value= Constant.int value}
