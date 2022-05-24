(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

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

(** C: operators *)

(** {1 Binary operators} *)

(** {2 Assignment operators}

    These form a subset of the binary operators, but float out into a
    separate enumeration for grouping purposes. *)

module Assign : sig
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
  [@@deriving sexp]

  include Ast_basic_types.Ast_node with type t := t
end

(** Enumeration of binary operators. *)
module Bin : sig
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
  [@@deriving sexp]

  include Ast_basic_types.Ast_node with type t := t

  val binds_same : t -> t -> bool
  (** [binds_same x y] is true provided that [x] has the same precedence
      level as [y]. *)

  val binds_tighter : t -> than:t -> bool
  (** [binds_tighter this ~than] is true provided that [this] binds tighter
      (has a strictly lower precedence level) than [than]. *)
end

(** {1 Unary operators} *)

(** {2 Prefix operators} *)
module Pre : sig
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
  [@@deriving sexp]

  include Ast_basic_types.Ast_node with type t := t
end

(** {2 Postfix operators} *)

module Post : sig
  type t = [`Inc (* ++ *) | `Dec (* -- *)] [@@deriving sexp]

  include Ast_basic_types.Ast_node with type t := t
end
