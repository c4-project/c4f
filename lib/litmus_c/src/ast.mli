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

open Ast_basic

module rec Expr : (Ast_types.S_expr with type Ty.t = Type_name.t)

and Enumerator : sig
  type t = {name: Identifier.t; value: Expr.t option} [@@deriving sexp]
end

and Enum_spec :
  (Ast_types.S_composite_spec
    with type kind := [`Enum]
     and type decl := Enumerator.t)

and Struct_decl :
  (Ast_types.S_g_decl
    with type qual := Spec_or_qual.t
     and type decl := Struct_declarator.t list)

and Type_spec :
  (Ast_types.S_type_spec
    with type su := Struct_or_union_spec.t
     and type en := Enum_spec.t)

and Spec_or_qual :
  (Ast_basic_types.Ast_node with type t = [Type_spec.t | Type_qual.t])

and Decl_spec :
  (Ast_basic_types.Ast_node
    with type t = [Storage_class_spec.t | Type_spec.t | Type_qual.t])

and Type_name :
  (Ast_types.S_g_decl
    with type qual := Spec_or_qual.t
     and type decl := Abs_declarator.t option)

and Struct_or_union_spec :
  (Ast_types.S_composite_spec
    with type kind := [`Struct | `Union]
     and type decl := Struct_decl.t)

and Param_decl :
  (Ast_types.S_g_decl
    with type qual := Decl_spec.t
     and type decl :=
      [`Concrete of Declarator.t | `Abstract of Abs_declarator.t option])

and Param_type_list : sig
  type t = {params: Param_decl.t list; style: [`Normal | `Variadic]}
  [@@deriving sexp]
end

and Direct_declarator :
  (Ast_types.S_direct_declarator
    with type dec := Declarator.t
     and type par := Param_type_list.t
     and type expr := Expr.t)

and Declarator :
  (Ast_types.S_declarator with type ddec := Direct_declarator.t)

and Struct_declarator :
  (Ast_types.S_struct_declarator
    with type dec := Declarator.t
     and type expr := Expr.t)

and Direct_abs_declarator :
  (Ast_types.S_direct_abs_declarator
    with type dec := Abs_declarator.t
     and type par := Param_type_list.t
     and type expr := Expr.t)

and Abs_declarator :
  (Ast_types.S_abs_declarator with type ddec := Direct_abs_declarator.t)

module Initialiser : sig
  type t = Assign of Expr.t | List of t list [@@deriving sexp]

  include Ast_basic_types.Ast_node with type t := t
end

module Init_declarator : sig
  type t = {declarator: Declarator.t; initialiser: Initialiser.t option}
  [@@deriving sexp]

  include Ast_basic_types.Ast_node with type t := t
end

module Decl :
  Ast_types.S_g_decl
    with type qual := Decl_spec.t
     and type decl := Init_declarator.t list

module Label : Ast_types.S_label with type expr := Expr.t

module rec Stm :
  (Ast_types.S_stm
    with type com := Compound_stm.t
     and type expr := Expr.t
     and type lbl := Label.t)

and Compound_stm :
  (Ast_types.S_compound_stm with type decl := Decl.t and type stm := Stm.t)

module Function_def : sig
  type t =
    { decl_specs: Decl_spec.t list
    ; signature: Declarator.t
    ; decls: Decl.t list
    ; body: Compound_stm.t }
  [@@deriving sexp]

  include Ast_basic_types.Ast_node with type t := t
end

module External_decl : sig
  type t = [`Fun of Function_def.t | `Decl of Decl.t] [@@deriving sexp]

  include Ast_basic_types.Ast_node with type t := t
end

module Translation_unit : sig
  type t = External_decl.t list [@@deriving sexp]

  include Ast_basic_types.Ast_node with type t := t
end

module Litmus_lang :
  C4f_litmus.Test_types.Basic
    with type Statement.t = [`Stm of Stm.t | `Decl of Decl.t]
     and type Program.t = Function_def.t
     and type Constant.t = Constant.t

module Litmus : sig
  include
    C4f_litmus.Test_types.S
      with module Lang := Litmus_lang
       and type raw = C4f_litmus.Test.Raw.M(Litmus_lang).t

  module Id : module type of C4f_common.Litmus_id
end
