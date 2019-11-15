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

(** Basic signatures and modules for the C AST.

    We declare these separately from the {{!Ast} rest of the AST} to break
    dependency cycles. *)

open Base

(** {2 Enumerations} *)

(** Enumeration of type qualifiers. *)
module Type_qual : sig
  type t = [`Const | `Volatile] [@@deriving sexp]

  include Ast_basic_types.Ast_node with type t := t

  include Act_utils.Enum_types.Extension_table with type t := t
end

(** Enumeration of primitive types. *)
module Prim_type : sig
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
  [@@deriving sexp]

  include Ast_basic_types.Ast_node with type t := t

  include Act_utils.Enum_types.Extension_table with type t := t
end

(** Enumeration of storage class specifiers. *)
module Storage_class_spec : sig
  type t = [`Auto | `Register | `Static | `Extern | `Typedef]
  [@@deriving sexp]

  include Ast_basic_types.Ast_node with type t := t

  include Act_utils.Enum_types.Extension_table with type t := t
end

(** {2 Basic AST nodes} *)

(** A slightly modified version of string with a quickcheck generator that
    only generates single-character-wide characters. *)
module Char_string : sig
  type t = string [@@deriving sexp, equal, compare, quickcheck]
end

(** AST node for constants *)
module Constant : sig
  type t = Char of Char_string.t | Float of float | Integer of int
  [@@deriving sexp, quickcheck]

  include Ast_basic_types.Ast_node with type t := t

  val to_int : t -> int Or_error.t

  (** {3 Quickcheck} *)

  val gen_int32_as_int : int Base_quickcheck.Generator.t
  (** [gen_int32_as_int] generates an [int] whose domain is that of [int32].
      This is useful for making sure that we don't generate integers that
      could overflow when running tests on 32-bit platforms. *)

  val gen_int32_constant : t Base_quickcheck.Generator.t
  (** [gen_int32_constant] generates an integer constant using
      {{!gen_int32_as_int} gen_int32_as_int}. *)
end

(** AST node for identifiers *)
module Identifier : sig
  include module type of Act_common.C_id

  include Ast_basic_types.Ast_node_with_identifier with type t := t
end

module Pointer : Ast_basic_types.Ast_node with type t = Type_qual.t list list
(** Ast node for pointers *)

(** Reusable AST building block for array subscripts *)
module Array : sig
  type ('a, 'i) t = {array: 'a; index: 'i} [@@deriving sexp, eq, compare]

  val pp : 'a Fmt.t -> 'i Fmt.t -> ('a, 'i) t Fmt.t

  module type S = sig
    type arr
    (** Type of arrays. *)

    type idx
    (** Type of indices. *)

    type nonrec t = (arr, idx) t [@@deriving sexp]

    include Ast_basic_types.Ast_node with type t := t
  end

  module Make (A : Ast_basic_types.Ast_node) (I : Ast_basic_types.Ast_node) :
    S with type arr := A.t and type idx := I.t
end
