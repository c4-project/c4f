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

open Ast_basic

include module type of Ast_intf

module rec Expr : (S_expr with type Ty.t = Type_name.t)

and Enumerator : sig
  type t = {name: Identifier.t; value: Expr.t option} [@@deriving sexp]
end

and Enum_spec :
  (S_composite_spec with type kind := [`Enum] and type decl := Enumerator.t)

and Struct_decl :
  (S_g_decl
    with type qual := Spec_or_qual.t
     and type decl := Struct_declarator.t list)

and Type_spec :
  (S_type_spec
    with type su := Struct_or_union_spec.t
     and type en := Enum_spec.t)

and Spec_or_qual : (Ast_node with type t = [Type_spec.t | Type_qual.t])

and Decl_spec :
  (Ast_node
    with type t = [Storage_class_spec.t | Type_spec.t | Type_qual.t])

and Type_name :
  (S_g_decl
    with type qual := Spec_or_qual.t
     and type decl := Abs_declarator.t option)

and Struct_or_union_spec :
  (S_composite_spec
    with type kind := [`Struct | `Union]
     and type decl := Struct_decl.t)

and Param_decl :
  (S_g_decl
    with type qual := Decl_spec.t
     and type decl :=
          [`Concrete of Declarator.t | `Abstract of Abs_declarator.t option])

and Param_type_list : sig
  type t = {params: Param_decl.t list; style: [`Normal | `Variadic]}
  [@@deriving sexp]
end

and Direct_declarator :
  (S_direct_declarator
    with type dec := Declarator.t
     and type par := Param_type_list.t
     and type expr := Expr.t)

and Declarator : (S_declarator with type ddec := Direct_declarator.t)

and Struct_declarator :
  (S_struct_declarator
    with type dec := Declarator.t
     and type expr := Expr.t)

and Direct_abs_declarator :
  (S_direct_abs_declarator
    with type dec := Abs_declarator.t
     and type par := Param_type_list.t
     and type expr := Expr.t)

and Abs_declarator :
  (S_abs_declarator with type ddec := Direct_abs_declarator.t)

module Initialiser : sig
  type t = Assign of Expr.t | List of t list [@@deriving sexp]

  include Ast_node with type t := t
end

module Init_declarator : sig
  type t = {declarator: Declarator.t; initialiser: Initialiser.t option}
  [@@deriving sexp]

  include Ast_node with type t := t
end

module Decl :
  S_g_decl
    with type qual := Decl_spec.t
     and type decl := Init_declarator.t list

module Label : S_label with type expr := Expr.t

module rec Stm :
  (S_stm
    with type com := Compound_stm.t
     and type expr := Expr.t
     and type lbl := Label.t)

and Compound_stm :
  (S_compound_stm with type decl := Decl.t and type stm := Stm.t)

module Function_def : sig
  type t =
    { decl_specs: Decl_spec.t list
    ; signature: Declarator.t
    ; decls: Decl.t list
    ; body: Compound_stm.t }
  [@@deriving sexp]

  include Ast_node with type t := t
end

module External_decl : sig
  type t = [`Fun of Function_def.t | `Decl of Decl.t] [@@deriving sexp]

  include Ast_node with type t := t
end

module Translation_unit : sig
  type t = External_decl.t list [@@deriving sexp]

  include Ast_node with type t := t
end

module Litmus_lang :
  Act_litmus.Ast.Basic
    with type Statement.t = [`Stm of Stm.t | `Decl of Decl.t]
     and type Program.t = Function_def.t
     and type Constant.t = Constant.t

module Litmus : sig
  include Act_litmus.Ast.S with module Lang := Litmus_lang

  module Id : module type of Act_common.Litmus_id
end
