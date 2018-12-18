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

module Identifier : sig
  type t = string
  [@@deriving sexp]
end

module Constant : sig
  type t =
    | Char    of char
    | Float   of float
    | Integer of int
  [@@deriving sexp]
  ;;
end

module Operator : sig
  type assign =
    [ `Assign
    | `Assign_mul
    | `Assign_div
    | `Assign_mod
    | `Assign_add
    | `Assign_sub
    | `Assign_shl
    | `Assign_shr
    | `Assign_and
    | `Assign_xor
    | `Assign_or
    ]
  [@@deriving sexp]
  ;;

  type bin =
    [ assign
    | `Comma
    | `Mul
    | `Div
    | `Mod
    | `Add
    | `Sub
    | `Shl
    | `Shr
    | `And
    | `Xor
    | `Or
    | `Land
    | `Lor
    | `Lt
    | `Le
    | `Eq
    | `Ge
    | `Gt
    | `Ne
    ]
  [@@deriving sexp]
  ;;

  type pre =
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

  type post =
    [ `Inc (* ++ *)
    | `Dec (* -- *)
    ]
  [@@deriving sexp]
  ;;
end

module Type_qual : sig
  type t =
    [ `Const
    | `Volatile
    ]
  [@@deriving sexp]
  ;;
end

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
end

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
end

module type S_decl = sig
  type q
  type d

  type t =
    { qualifiers : q list
    ; declarator : d
    }
  [@@deriving sexp]
  ;;
end

module type S_expr = sig
  type ty

  type t =
    | Prefix      of Operator.pre * t
    | Postfix     of t * Operator.post
    | Binary      of t * Operator.bin * t
    | Ternary     of { cond   : t
                     ; t_expr : t
                     ; f_expr : t
                     }
    | Cast        of ty * t
    | Call        of { func : t; arguments : t list}
    | Subscript   of { array : t; index : t }
    | Field       of { value  : t
                     ; field  : Identifier.t
                     ; access : [ `Direct (* . *) | `Deref (* -> *) ]
                     }
    | Sizeof_type of ty
    | Identifier  of Identifier.t
    | String      of String.t
    | Constant    of Constant.t
    | Brackets    of t
  [@@deriving sexp]
  ;;
end

module Pointer : sig
  type t = (Type_qual.t list) list
  [@@deriving sexp]
end

module type S_direct_declarator = sig
  type dec
  type par
  type expr

  type t =
    | Id of Identifier.t
    | Bracket of dec
    | Array of t * expr option
    | Fun_decl of t * par
    | Fun_call of t * Identifier.t list
  [@@deriving sexp]
  ;;
end

module type S_declarator = sig
  type ddec

  type t =
    { pointer : Pointer.t option
    ; direct  : ddec
    }
  [@@deriving sexp]
  ;;
end

module type S_struct_declarator = sig
  type dec
  type expr

  type t =
    | Regular of dec
    | Bitfield of dec option * expr
  [@@deriving sexp]
  ;;
end

module type S_type_spec = sig
  type su
  type en

  type t =
    [ Prim_type.t
    | `Struct_or_union of su
    | `Enum of en
    | `Defined_type of Identifier.t
    ]
  [@@deriving sexp]
  ;;
end

module type S_composite_spec = sig
  type ty
  type dec

  type t =
    | Literal of
        { ty       : ty
        ; name_opt : Identifier.t option
        ; decls    : dec list
        }
    | Named of ty * Identifier.t
  [@@deriving sexp]
  ;;
end

module type S_direct_abs_declarator = sig
  type dec
  type par
  type expr

  type t =
    | Bracket of dec
    | Array of t option * expr option
    | Fun_decl of t option * par option
  [@@deriving sexp]
  ;;
end

module type S_abs_declarator = sig
  type ddec

  type t =
    | Pointer of Pointer.t
    | Direct of Pointer.t option * ddec
  [@@deriving sexp]
  ;;
end

module rec Expr
  : (S_expr with type ty := Type_name.t)
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
     with type ty  := [`Enum]
      and type dec := Enumerator.t)
and Struct_decl
  : (S_decl
     with type q := [ Type_spec.t | Type_qual.t ]
      and type d := Struct_declarator.t list)
and Type_spec
  : (S_type_spec
     with type su := Struct_or_union_spec.t
      and type en := Enum_spec.t)
and Type_name
  : (S_decl
     with type q := [ Type_spec.t | Type_qual.t ]
      and type d := Abs_declarator.t option)
and Struct_or_union_spec
  : (S_composite_spec
     with type ty  := [`Struct | `Union]
      and type dec := Struct_decl.t)
and Decl_spec : sig
  type t =
    [ Storage_class_spec.t
    | Type_spec.t
    | Type_qual.t
    ]
  [@@deriving sexp]
  ;;
end
and Param_decl
  : (S_decl
     with type q := Decl_spec.t
      and type d := [ `Concrete of Declarator.t
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

module Decl : S_decl
  with type q := Decl_spec.t
   and type d := Init_declarator.t list
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

  type t =
    { decls : Decl.t list
    ; stms  : stm list
    }
  [@@deriving sexp]
  ;;
end

module rec Stm : (S_stm with type com := Compound_stm.t)
and Compound_stm : (S_compound_stm with type stm := Stm.t)
;;

module Function_def : sig
  type t =
    { decl_specs : Decl_spec.t list
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

module Litmus : sig
  module Id : sig
    type t =
      | Local of int * Identifier.t
      | Global of Identifier.t
    [@@deriving sexp]
    ;;
  end

  module Pred : sig
    type t =
      | Bracket of t
      | Or of t * t
      | And of t * t
      | Eq of Id.t * Constant.t
    [@@deriving sexp]
    ;;
  end

  module Post : sig
    type t =
      { quantifier : [ `Exists ]
      ; predicate  : Pred.t
      }
    [@@deriving sexp]
    ;;
  end

  module Decl : sig
    type t =
      [ External_decl.t
      | `Init of Expr.t list
      | `Post of Post.t
      ]
    [@@deriving sexp]
    ;;
  end

  module Test : sig
    type t =
      { language : string
      ; name     : string
      ; decls    : Decl.t list
      }
    [@@deriving sexp]
    ;;
  end
end
