(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor (parts (c) 2010-2018 Institut
   National de Recherche en Informatique et en Automatique, Jade
   Alglave, and Luc Maranget)

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
   SOFTWARE.

   This file derives from the Herd7 project
   (https://github.com/herd/herdtools7); its original attribution and
   copyright notice follow. *)

(****************************************************************************)
(*                           the diy toolsuite                              *)
(*                                                                          *)
(* Jade Alglave, University College London, UK.                             *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                          *)
(*                                                                          *)
(* Copyright 2010-present Institut National de Recherche en Informatique et *)
(* en Automatique and the authors. All rights reserved.                     *)
(*                                                                          *)
(* This software is governed by the CeCILL-B license under French law and   *)
(* abiding by the rules of distribution of free software. You can use,      *)
(* modify and/ or redistribute the software under the terms of the CeCILL-B *)
(* license as circulated by CEA, CNRS and INRIA at the following URL        *)
(* "http://www.cecill.info". We also give a copy in LICENSE.txt.            *)
(****************************************************************************)

(** Basic signatures and modules for the C AST.

    We declare these separately from the {{!Ast}rest of the AST} to
    break dependency cycles. *)

open Base

include module type of Ast_basic_intf
(** As usual, the signatures are in a separate 'intf' module. *)

(** {2 Enumerations} *)

(** Operator enumerations. *)
module Operators : sig
  (** Enumeration of assignment operators. *)
  module Assign : sig
    type t =
      [ `Assign     (*   = *)
      | `Assign_mul (*  *= *)
      | `Assign_div (*  /= *)
      | `Assign_mod (*  %= *)
      | `Assign_add (*  += *)
      | `Assign_sub (*  -= *)
      | `Assign_shl (* <<= *)
      | `Assign_shr (* >>= *)
      | `Assign_and (*  &= *)
      | `Assign_xor (*  ^= *)
      | `Assign_or  (*  |= *)
      ]
    [@@deriving sexp]
    ;;

    include Ast_node with type t := t
  end

  (** Enumeration of binary operators. *)
  module Bin : sig
    type t =
      [ Assign.t
      | `Comma (* ,  *)
      | `Mul   (* *  *)
      | `Div   (* /  *)
      | `Mod   (* %  *)
      | `Add   (* +  *)
      | `Sub   (* -  *)
      | `Shl   (* << *)
      | `Shr   (* >> *)
      | `And   (* &  *)
      | `Xor   (* ^  *)
      | `Or    (* |  *)
      | `Land  (* && *)
      | `Lor   (* || *)
      | `Lt    (* <  *)
      | `Le    (* <= *)
      | `Eq    (* == *)
      | `Ge    (* >= *)
      | `Gt    (* >  *)
      | `Ne    (* != *)
      ]
    [@@deriving sexp]
    ;;

    include Ast_node with type t := t
  end

  (** Enumeration of prefix operators. *)
  module Pre : sig
    type t =
      [ `Inc        (* ++ *)
      | `Dec        (* -- *)
      | `Sizeof_val (* sizeof *)
      | `Ref        (* & *)
      | `Deref      (* * *)
      | `Add        (* + *)
      | `Sub        (* - *)
      | `Not        (* ~ *)
      | `Lnot       (* ! *)
      ]
    [@@deriving sexp]
    ;;

    include Ast_node with type t := t
  end

  (** Enumeration of postfix operators. *)
  module Post : sig
    type t =
      [ `Inc (* ++ *)
      | `Dec (* -- *)
      ]
    [@@deriving sexp]
    ;;

    include Ast_node with type t := t
  end
end

(** Enumeration of type qualifiers. *)
module Type_qual : sig
  type t =
    [ `Const
    | `Volatile
    ]
  [@@deriving sexp]
  ;;

  include Ast_node with type t := t
  include Utils.Enum.Extension_table with type t := t
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
    | `Unsigned
    ]
  [@@deriving sexp]
  ;;

  include Ast_node with type t := t
  include Utils.Enum.Extension_table with type t := t
end

(** Enumeration of storage class specifiers. *)
module Storage_class_spec : sig
  type t =
    [ `Auto
    | `Register
    | `Static
    | `Extern
    | `Typedef
    ]
  [@@deriving sexp]
  ;;

  include Ast_node with type t := t
  include Utils.Enum.Extension_table with type t := t
end

(** {2 Basic AST nodes} *)

(** AST node for constants *)
module Constant : sig
  type t =
    | Char    of char
    | Float   of float
    | Integer of int
  [@@deriving sexp]
  ;;
  include Ast_node with type t := t
end

(** AST node for identifiers *)
module Identifier : sig
  type t = string [@@deriving eq]
  include Ast_node_with_identifier with type t := t
end

(** Ast node for pointers *)
module Pointer : Ast_node with type t = (Type_qual.t list) list

(** Reusable AST building block for array subscripts *)
module Array : sig
  type ('a, 'i) t = { array : 'a; index : 'i } [@@deriving sexp]

  val pp : 'a Fmt.t -> 'i Fmt.t -> ('a, 'i) t Fmt.t

  module type S = sig
    type arr (** Type of arrays *)
    type idx (** Type of indices *)

    type nonrec t = (arr, idx) t [@@deriving sexp]

    include Ast_node with type t := t
  end

  module Make (A : Ast_node) (I : Ast_node) : S
    with type arr := A.t and type idx := I.t
end
