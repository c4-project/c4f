(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Base_quickcheck

type t = {ty: Type.t; value: Constant.t option}
[@@deriving sexp, make, compare, equal, fields, quickcheck]

let of_int ?(is_atomic : bool = false) ?(is_volatile : bool = false)
    (value : int) =
  make
    ~ty:(Type.int ~is_atomic ~is_volatile ())
    ~value:(Constant.int value) ()
