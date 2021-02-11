(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(* Parts of this file ultimately derive from the Herdtools7 C AST, which has
   the following attribution:

   the diy toolsuite

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

module Assign = struct
  type t =
    [ `Assign (* = *)
    | `Assign_mul (* *= *)
    | `Assign_div (* /= *)
    | `Assign_mod (* %= *)
    | `Assign_add (* += *)
    | `Assign_sub (* -= *)
    | `Assign_shl (* <<= *)
    | `Assign_shr (* >>= *)
    | `Assign_and (* &= *)
    | `Assign_xor (* ^= *)
    | `Assign_or (* |= *) ]
  [@@deriving sexp, equal, compare]

  let to_string : t -> string = function
    | `Assign -> "="
    | `Assign_mul -> "*="
    | `Assign_div -> "/="
    | `Assign_mod -> "%="
    | `Assign_add -> "+="
    | `Assign_sub -> "-="
    | `Assign_shl -> "<<="
    | `Assign_shr -> ">>="
    | `Assign_and -> "&="
    | `Assign_xor -> "^="
    | `Assign_or -> "|="

  let pp : t Fmt.t = Fmt.of_to_string to_string
end

module Bin = struct
  type t =
    [ Assign.t
    | `Comma (* , *)
    | `Mul (* * *)
    | `Div (* / *)
    | `Mod (* % *)
    | `Add (* + *)
    | `Sub (* - *)
    | `Shl (* << *)
    | `Shr (* >> *)
    | `And (* & *)
    | `Xor (* ^ *)
    | `Or (* | *)
    | `Land (* && *)
    | `Lor (* || *)
    | `Lt (* < *)
    | `Le (* <= *)
    | `Eq (* == *)
    | `Ge (* >= *)
    | `Gt (* > *)
    | `Ne (* != *) ]
  [@@deriving sexp, equal, compare]

  (* See https://en.cppreference.com/w/c/language/operator_precedence *)

  let precedence : t -> int = function
    | `Mul | `Div | `Mod -> 3
    | `Add | `Sub -> 4
    | `Shl | `Shr -> 5
    | `Lt | `Le | `Gt | `Ge -> 6
    | `Eq | `Ne -> 7
    | `And -> 8
    | `Xor -> 9
    | `Or -> 10
    | `Land -> 11
    | `Lor -> 12
    | #Assign.t -> 14
    | `Comma -> 15

  let binds_same : t -> t -> bool = Comparable.lift Int.equal ~f:precedence

  let binds_tighter (this : t) ~(than : t) : bool =
    Comparable.lift Int.( < ) ~f:precedence this than

  let to_string : t -> string = function
    | #Assign.t as a -> Assign.to_string a
    | `Comma -> ","
    | `Mul -> "*"
    | `Div -> "/"
    | `Mod -> "%"
    | `Add -> "+"
    | `Sub -> "-"
    | `Shl -> "<<"
    | `Shr -> ">>"
    | `And -> "&"
    | `Xor -> "^"
    | `Or -> "|"
    | `Land -> "&&"
    | `Lor -> "||"
    | `Lt -> "<"
    | `Le -> "<="
    | `Eq -> "=="
    | `Ge -> ">="
    | `Gt -> ">"
    | `Ne -> "!="

  let pp : t Fmt.t = Fmt.of_to_string to_string
end

module Pre = struct
  type t =
    [ `Inc (* ++ *)
    | `Dec (* -- *)
    | `Sizeof_val (* sizeof *)
    | `Ref (* & *)
    | `Deref (* * *)
    | `Add (* + *)
    | `Sub (* - *)
    | `Not (* ~ *)
    | `Lnot (* ! *) ]
  [@@deriving sexp, equal, compare]

  let to_string : t -> string = function
    | `Inc -> "++"
    | `Dec -> "--"
    | `Sizeof_val -> "sizeof"
    | `Ref -> "&"
    | `Deref -> "*"
    | `Add -> "+"
    | `Sub -> "-"
    | `Not -> "~"
    | `Lnot -> "!"

  let pp : t Fmt.t = Fmt.of_to_string to_string
end

module Post = struct
  type t = [`Inc (* ++ *) | `Dec (* -- *)] [@@deriving sexp, equal, compare]

  let to_string : t -> string = function `Inc -> "++" | `Dec -> "--"

  let pp : t Fmt.t = Fmt.of_to_string to_string
end
