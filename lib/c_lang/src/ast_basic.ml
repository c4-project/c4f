(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor (parts (c) 2010-2018 Institut National
   de Recherche en Informatique et en Automatique, Jade Alglave, and Luc
   Maranget)

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

   This file derives from the Herd7 project
   (https://github.com/herd/herdtools7); its original attribution and
   copyright notice follow. *)

(****************************************************************************)
(* the diy toolsuite *)
(*  *)
(* Jade Alglave, University College London, UK. *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France. *)
(*  *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved. *)
(*  *)
(* This software is governed by the CeCILL-B license under French law and *)
(* abiding by the rules of distribution of free software. You can use, *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt. *)
(****************************************************************************)

open Core_kernel

module Type_qual = struct
  module M = struct
    type t = [`Const | `Volatile] [@@deriving sexp, enum]

    let table : (t, string) List.Assoc.t =
      [(`Const, "const"); (`Volatile, "volatile")]
  end

  include M
  include Act_utils.Enum.Extend_table (M)
end

module Prim_type = struct
  module M = struct
    type t =
      [ `Void
      | `Char
      | `Short
      | `Int
      | `Long
      | `Float
      | `Double
      | `Signed
      | `Unsigned ]
    [@@deriving sexp, enum]

    let table : (t, string) List.Assoc.t =
      [ (`Void, "void")
      ; (`Char, "char")
      ; (`Short, "short")
      ; (`Int, "int")
      ; (`Long, "long")
      ; (`Float, "float")
      ; (`Double, "double")
      ; (`Signed, "signed")
      ; (`Unsigned, "unsigned") ]
  end

  include M
  include Act_utils.Enum.Extend_table (M)
end

module Storage_class_spec = struct
  module M = struct
    type t = [`Auto | `Register | `Static | `Extern | `Typedef]
    [@@deriving sexp, enum]

    let table : (t, string) List.Assoc.t =
      [ (`Auto, "auto")
      ; (`Register, "register")
      ; (`Static, "static")
      ; (`Extern, "extern")
      ; (`Typedef, "typedef") ]
  end

  include M
  include Act_utils.Enum.Extend_table (M)
end

module Array = struct
  type ('a, 'i) t = {array: 'a; index: 'i} [@@deriving sexp, eq, compare]

  let pp (ppa : 'a Fmt.t) (ppi : 'i Fmt.t) : ('a, 'i) t Fmt.t =
    Fmt.(
      using
        (fun {array; index} -> (array, index))
        (pair ~sep:nop ppa (brackets ppi)))

  module type S = sig
    type arr
    (** Type of arrays. *)

    type idx
    (** Type of indices. *)

    type nonrec t = (arr, idx) t

    include Ast_basic_types.Ast_node with type t := t
  end

  module Make (A : Ast_basic_types.Ast_node) (I : Ast_basic_types.Ast_node) :
    S with type arr := A.t and type idx := I.t = struct
    type nonrec t = (A.t, I.t) t

    let t_of_sexp = t_of_sexp A.t_of_sexp I.t_of_sexp

    let sexp_of_t = sexp_of_t A.sexp_of_t I.sexp_of_t

    let equal = equal A.equal I.equal

    let compare = compare A.compare I.compare

    let pp = pp A.pp I.pp
  end
end

module Char_string = struct
  type t = string [@@deriving sexp, equal, compare, quickcheck]

  let quickcheck_generator : t Base_quickcheck.Generator.t =
    (* TODO(@MattWindsor91): Unicode? *)
    Base_quickcheck.Generator.map ~f:String.of_char
      Base_quickcheck.Generator.char_print

  let quickcheck_shrinker : t Base_quickcheck.Shrinker.t =
    Base_quickcheck.Shrinker.atomic
end

module Float_not_nan = struct
  type t = float [@@deriving sexp, equal, compare, quickcheck]

  let quickcheck_generator : t Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.float_finite
end

module Constant = struct
  type t =
    | Char of Char_string.t
    (* UTF-8 *)
    | Float of Float_not_nan.t
    | Integer of int
  [@@deriving sexp, variants, equal, compare, quickcheck]

  let pp_char =
    Fmt.(quote ~mark:"'" (using Act_utils.Lex_utils.escape_string string))

  let pp f = function
    | Char c ->
        pp_char f c
    | Float d ->
        (* NOT Fmt.float; it emits, for instance, '-0' instead of '-0.'. This
           then breaks parser round-tripping. *)
        Fmt.(using Float.to_string string) f d
    | Integer i ->
        Fmt.int f i

  let gen_int32_as_int : int Quickcheck.Generator.t =
    Quickcheck.Generator.map [%quickcheck.generator: int32] ~f:(fun x ->
        Option.value ~default:0 (Int.of_int32 x))

  let gen_int32_constant : t Quickcheck.Generator.t =
    Quickcheck.Generator.map ~f:integer gen_int32_as_int

  let to_int : t -> int Or_error.t = function
    | Integer k ->
        Or_error.return k
    | Char _ ->
        Or_error.error_string "expected integer literal, got char"
    | Float _ ->
        Or_error.error_string "expected integer literal, got float"
end

module Identifier = struct
  include Act_common.C_id

  let identifier = Fn.id
end

module Pointer :
  Ast_basic_types.Ast_node with type t = Type_qual.t list list = struct
  type t = Type_qual.t list list [@@deriving sexp, eq, compare]

  let pp : t Fmt.t =
    Fmt.(list ~sep:sp (any "*" ++ list ~sep:sp Type_qual.pp))
end
