(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = Break | Continue | Return
[@@deriving sexp, compare, equal, quickcheck]

let in_loop_only : t -> bool = function
  | Break | Continue ->
      true
  | Return ->
      false
