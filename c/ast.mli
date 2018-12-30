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

open Ast_basic
include module type of Ast_intf

module rec Expr : (S_expr with module Ty := Type_name)
and Enumerator : sig
  type t =
    { name  : Identifier.t
    ; value : Expr.t option
    }
  [@@deriving sexp]
  ;;
end
and Enum_spec
  : (S_composite_spec
     with type kind := [`Enum]
      and type decl := Enumerator.t)
and Struct_decl
  : (S_g_decl
     with type qual := [ Type_spec.t | Type_qual.t ]
      and type decl := Struct_declarator.t list)
and Type_spec
  : (S_type_spec
     with type su := Struct_or_union_spec.t
      and type en := Enum_spec.t)
and Type_name
  : (S_g_decl
     with type qual := [ Type_spec.t | Type_qual.t ]
      and type decl := Abs_declarator.t option)
and Struct_or_union_spec
  : (S_composite_spec
     with type kind := [`Struct | `Union]
      and type decl := Struct_decl.t)
and Param_decl
  : (S_g_decl
     with type qual :=
       [ Storage_class_spec.t | Type_spec.t | Type_qual.t ]
      and type decl :=
        [ `Concrete of Declarator.t
        | `Abstract of Abs_declarator.t option
        ])
and Param_type_list : sig
  type t =
    { params : Param_decl.t list
    ; style  : [`Normal | `Variadic]
    }
  [@@deriving sexp]
  ;;
end
and Direct_declarator
  : (S_direct_declarator
     with type dec  := Declarator.t
      and type par  := Param_type_list.t
      and type expr := Expr.t)
and Declarator
  : (S_declarator
     with type ddec := Direct_declarator.t)
and Struct_declarator
    : (S_struct_declarator
       with type dec  := Declarator.t
        and type expr := Expr.t)
and Direct_abs_declarator
  : (S_direct_abs_declarator
     with type dec  := Abs_declarator.t
      and type par  := Param_type_list.t
      and type expr := Expr.t)
and Abs_declarator
  : (S_abs_declarator with type ddec := Direct_abs_declarator.t)
;;

module Initialiser : sig
  type t =
    | Assign of Expr.t
    | List of t list
  [@@deriving sexp]
  ;;
end

module Init_declarator : sig
  type t =
    { declarator : Declarator.t
    ; initialiser : Initialiser.t option
    }
  [@@deriving sexp]
  ;;
end

module Decl : S_g_decl
  with type qual := [ Storage_class_spec.t | Type_spec.t | Type_qual.t ]
   and type decl := Init_declarator.t list
;;

module Label : sig
  type t =
    | Normal of Identifier.t
    | Case   of Expr.t
    | Default
  [@@deriving sexp]
  ;;
end

module type S_stm = sig
  type com

  type t =
    | Label of Label.t * t
    | Expr of Expr.t option
    | Compound of com
    | If of
        { cond : Expr.t
        ; t_branch : t
        ; f_branch : t option
        }
    | Switch of Expr.t * t
    | While of Expr.t * t
    | Do_while of t * Expr.t
    | For of
        { init   : Expr.t option
        ; cond   : Expr.t option
        ; update : Expr.t option
        ; body   : t
        }
    | Goto of Identifier.t
    | Continue
    | Break
    | Return of Expr.t option
  [@@deriving sexp]
  ;;
end

module type S_compound_stm = sig
  type stm

  (* TODO(@MattWindsor91): this is the C99 definition of compound
     statements, but everything else (including the parser!) targets
     C89. *)

  type t = [`Stm of stm | `Decl of Decl.t] list [@@deriving sexp]
end

module rec Stm : (S_stm with type com := Compound_stm.t)
and Compound_stm : (S_compound_stm with type stm := Stm.t)
;;

module Function_def : sig
  type t =
    { decl_specs : [ Storage_class_spec.t | Type_spec.t | Type_qual.t ] list
    ; signature  : Declarator.t
    ; decls      : Decl.t list
    ; body       : Compound_stm.t
    }
  [@@deriving sexp]
  ;;
end

module External_decl : sig
  type t =
    [ `Fun  of Function_def.t
    | `Decl of Decl.t
    ]
  [@@deriving sexp]
  ;;
end

module Translation_unit : sig
  type t = External_decl.t list
  [@@deriving sexp]
  ;;
end

module Litmus_lang : Litmus.Ast.Basic
  with type Statement.t = [`Stm of Stm.t | `Decl of Decl.t]
   and type Program.t = Function_def.t
   and type Constant.t = Constant.t
;;

module Litmus : Litmus.Ast.S with module Lang := Litmus_lang

