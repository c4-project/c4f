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

open Base
open Ast_basic

(** {2 Signatures of specific nodes} *)

(** {3 Signatures shared between multiple nodes} *)

(** Signature of general declaration nodes. *)
module type S_g_decl = sig
  (** Type of qualifiers. *)
  type qual

  (** Type of declarators. *)
  type decl

  type t = {qualifiers: qual list; declarator: decl}

  include Ast_basic_types.Ast_node with type t := t
end

(** Signature of general composite (enum, struct, union) specifiers. *)
module type S_composite_spec = sig
  (** Type of kind of composite spec (eg. 'enum'). *)
  type kind

  (** Type of internal declarations. *)
  type decl

  type t =
    | Literal of {kind: kind; name_opt: Identifier.t option; decls: decl list}
    | Named of kind * Identifier.t

  include Ast_basic_types.Ast_node with type t := t
end

(** {3 Declarators} *)

(** Signature of direct declarators. *)
module type S_direct_declarator = sig
  (** Type of declarators. *)
  type dec

  (** Type of parameters. *)
  type par

  (** Type of expressions. *)
  type expr

  type t =
    | Id of Identifier.t
    | Bracket of dec
    | Array of (t, expr option) Array.t
    | Fun_decl of t * par
    | Fun_call of t * Identifier.t list

  include Ast_basic_types.Ast_node_with_identifier with type t := t
end

(** Signature of declarators. *)
module type S_declarator = sig
  (** Type of direct declarators. *)
  type ddec

  type t = {pointer: Pointer.t option; direct: ddec}

  include Ast_basic_types.Ast_node_with_identifier with type t := t
end

(** Signature of direct abstract declarators. *)
module type S_direct_abs_declarator = sig
  (** Type of abstract declarators. *)
  type dec

  (** Type of parameters. *)
  type par

  (** Type of expressions. *)
  type expr

  type t =
    | Bracket of dec
    | Array of (t option, expr option) Array.t
    | Fun_decl of t option * par option

  include Ast_basic_types.Ast_node with type t := t
end

(** Signature of abstract declarators. *)
module type S_abs_declarator = sig
  (** Type of direct abstract declarators. *)
  type ddec

  type t = Pointer of Pointer.t | Direct of Pointer.t option * ddec

  include Ast_basic_types.Ast_node with type t := t
end

(** Signature of struct declarators. *)
module type S_struct_declarator = sig
  (** Type of declarations. *)
  type dec

  (** Type of expressions. *)
  type expr

  type t = Regular of dec | Bitfield of dec option * expr

  include Ast_basic_types.Ast_node with type t := t
end

(* {3 Other} *)

(** Signature of expression nodes. *)
module type S_expr = sig
  (** Type of type names. *)
  module Ty : Ast_basic_types.Ast_node

  type t =
    | Prefix of Operators.Pre.t * t
    | Postfix of t * Operators.Post.t
    | Binary of t * Operators.Bin.t * t
    | Ternary of {cond: t; t_expr: t; f_expr: t}
    | Cast of Ty.t * t
    | Call of {func: t; arguments: t list}
    | Subscript of (t, t) Array.t
    | Field of
        { value: t
        ; field: Identifier.t
        ; access: [`Direct (* . *) | `Deref (* -> *)] }
    | Sizeof_type of Ty.t
    | Identifier of Identifier.t
    | String of String.t
    | Constant of Constant.t
    | Brackets of t

  include Ast_basic_types.Ast_node with type t := t
end

(** Signature of labels *)
module type S_label = sig
  (** Type of expressions used in case labels. *)
  type expr

  type t = Normal of Identifier.t | Case of expr | Default

  include Ast_basic_types.Ast_node with type t := t
end

(** Signature of compound statements *)
module type S_compound_stm = sig
  (** Type of declarations. *)
  type decl

  (** Type of statements. *)
  type stm

  module Elt :
    Ast_basic_types.Ast_node with type t = [`Stm of stm | `Decl of decl]

  (* TODO(@MattWindsor91): this is the C99 definition of compound statements,
     but everything else targets C89. *)
  type t = Elt.t list

  include Ast_basic_types.Ast_node with type t := t
end

(** Signature of statements *)
module type S_stm = sig
  (** Type of compound statements. *)
  type com

  (** Type of expressions. *)
  type expr

  (** Type of labels. *)
  type lbl

  type t =
    | Label of lbl * t
    | Expr of expr option
    | Compound of com
    | Atomic of com  (** Memalloy style atomic block. *)
    | Synchronized of com  (** C++ TM style synchronized block. *)
    | If of {cond: expr; t_branch: t; f_branch: t option}
    | Switch of expr * t
    | While of expr * t
    | Do_while of t * expr
    | For of
        {init: expr option; cond: expr option; update: expr option; body: t}
    | Goto of Identifier.t
    | Continue
    | Break
    | Return of expr option

  include Ast_basic_types.Ast_node with type t := t
end

(** Signature of type specifiers *)
module type S_type_spec = sig
  (** Type of struct-or-union specifiers. *)
  type su

  (** Type of enum specifiers. *)
  type en

  type t =
    [ Prim_type.t
    | `Struct_or_union of su
    | `Enum of en
    | `Defined_type of Identifier.t ]

  include Ast_basic_types.Ast_node with type t := t
end

(** Signature of parameter type lists *)
module type S_param_type_list = sig
  (** Type of parameter declarations. *)
  type pdecl

  type t = {params: pdecl list; style: [`Normal | `Variadic]}

  include Ast_basic_types.Ast_node with type t := t
end
