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

open Core_kernel

(* New AST, under construction *)

module Identifier = struct
  type t = string
    [@@deriving sexp]
end

module Constant = struct
  type t =
    | Char    of char
    | Float   of float
    | Integer of int
  [@@deriving sexp]
  ;;
end

module Operator = struct
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

module type S_decl = sig
  type qual
  type decl

  type t =
    { qualifiers : qual list
    ; declarator : decl
    }
  [@@deriving sexp]
  ;;
end

module type Basic_decl = sig
  type qual [@@deriving sexp] (** Type of qualifiers *)
  type decl [@@deriving sexp] (** Type of declarators *)
end

module Make_decl (B : Basic_decl) : S_decl
  with type qual := B.qual
   and type decl := B.decl = struct
  type t =
    { qualifiers : B.qual list
    ; declarator : B.decl
    }
  [@@deriving sexp]
  ;;
end

module Type_qual = struct
  type t =
    [ `Const
    | `Volatile
    ]
  [@@deriving sexp]
  ;;
end

module Prim_type = struct
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
end

module Storage_class_spec = struct
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

module Pointer = struct
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

  val identifier : t -> Identifier.t
end

module type S_declarator = sig
  type ddec

  type t =
    { pointer : Pointer.t option
    ; direct  : ddec
    }
  [@@deriving sexp]
  ;;

  val identifier : t -> Identifier.t
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

module type Basic_composite_spec = sig
  type ty [@@deriving sexp]
  type decl [@@deriving sexp]
end

module type S_composite_spec = sig
  type ty
  type decl

  type t =
    | Literal of
        { ty       : ty
        ; name_opt : Identifier.t option
        ; decls    : decl list
        }
    | Named of ty * Identifier.t
  [@@deriving sexp]
end

module Make_composite_spec (B : Basic_composite_spec)
  : S_composite_spec
    with type ty   := B.ty
     and type decl := B.decl = struct
  type t =
    | Literal of
        { ty       : B.ty
        ; name_opt : Identifier.t option
        ; decls    : B.decl list
        }
    | Named of B.ty * Identifier.t
  [@@deriving sexp]
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

module rec Expr : S_expr
  with type ty := Type_name.t = struct
  type t =
    | Prefix      of Operator.pre * t
    | Postfix     of t * Operator.post
    | Binary      of t * Operator.bin * t
    | Ternary     of { cond   : t
                     ; t_expr : t
                     ; f_expr : t
                     }
    | Cast        of Type_name.t * t
    | Call        of { func : t; arguments : t list}
    | Subscript   of { array : t; index : t }
    | Field       of { value  : t
                     ; field  : Identifier.t
                     ; access : [ `Direct (* . *) | `Deref (* -> *) ]
                     }
    | Sizeof_type of Type_name.t
    | Identifier  of Identifier.t
    | String      of String.t
    | Constant    of Constant.t
    | Brackets    of t
  [@@deriving sexp]
  ;;
end
and Enumerator : sig
  type t =
    { name  : Identifier.t
    ; value : Expr.t option
    }
  [@@deriving sexp]
  ;;
end = struct
  type t =
    { name  : Identifier.t
    ; value : Expr.t option
    }
  [@@deriving sexp]
  ;;
end
and Enum_spec : S_composite_spec
  with type ty  := [`Enum]
   and type decl := Enumerator.t = Make_composite_spec (struct
    type ty   = [`Enum]      [@@deriving sexp]
    type decl = Enumerator.t [@@deriving sexp]
  end)
and Struct_decl : S_decl
  with type qual := [ Type_spec.t | Type_qual.t ]
   and type decl := Struct_declarator.t list = Make_decl (struct
    type qual = [ Type_spec.t | Type_qual.t ] [@@deriving sexp]
    type decl = Struct_declarator.t list [@@deriving sexp]
  end)
and Type_spec : S_type_spec
  with type su := Struct_or_union_spec.t
   and type en := Enum_spec.t = struct
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
    | `Struct_or_union of Struct_or_union_spec.t
    | `Enum of Enum_spec.t
    | `Defined_type of Identifier.t
    ]
  [@@deriving sexp]
  ;;
end
and Type_name : S_decl
  with type qual := [ Type_spec.t | Type_qual.t ]
   and type decl := Abs_declarator.t option =
  Make_decl (struct
    type qual = [ Type_spec.t | Type_qual.t ] [@@deriving sexp]
    type decl = Abs_declarator.t option [@@deriving sexp]
  end)
and Struct_or_union_spec : S_composite_spec
  with type ty  := [`Struct | `Union]
   and type decl := Struct_decl.t = Make_composite_spec (struct
    type ty   = [`Struct | `Union] [@@deriving sexp]
    type decl = Struct_decl.t [@@deriving sexp]
end)
and Param_decl : S_decl
  with type qual := [ Storage_class_spec.t | Type_spec.t | Type_qual.t ]
   and type decl := [ `Concrete of Declarator.t
                    | `Abstract of Abs_declarator.t option
                    ] = Make_decl (struct
    type qual = [ Storage_class_spec.t | Type_spec.t | Type_qual.t ] [@@deriving sexp]
    type decl = [ `Concrete of Declarator.t
                | `Abstract of Abs_declarator.t option
                ] [@@deriving sexp]
  end)
and Param_type_list : sig
  type t =
    { params : Param_decl.t list
    ; style  : [`Normal | `Variadic]
    }
  [@@deriving sexp]
  ;;
end = struct
  type t =
    { params : Param_decl.t list
    ; style  : [`Normal | `Variadic]
    }
  [@@deriving sexp]
  ;;
end
and Direct_declarator : S_direct_declarator
  with type dec  := Declarator.t
   and type par  := Param_type_list.t
   and type expr := Expr.t = struct
  type t =
    | Id of Identifier.t
    | Bracket of Declarator.t
    | Array of t * Expr.t option
    | Fun_decl of t * Param_type_list.t
    | Fun_call of t * Identifier.t list
  [@@deriving sexp]

  let rec identifier = function
    | Id x -> x
    | Bracket d -> Declarator.identifier d
    | Array (t, _) -> identifier t
    | Fun_decl (t, _) -> identifier t
    | Fun_call (t, _) -> identifier t
  ;;
end
and Declarator : S_declarator
  with type ddec := Direct_declarator.t = struct
  type t =
    { pointer : Pointer.t option
    ; direct  : Direct_declarator.t
    }
  [@@deriving sexp]
  ;;

  let identifier { direct; _ } = Direct_declarator.identifier direct
end
and Struct_declarator : S_struct_declarator
  with type dec  := Declarator.t
   and type expr := Expr.t = struct
  type t =
    | Regular of Declarator.t
    | Bitfield of Declarator.t option * Expr.t
  [@@deriving sexp]
  ;;
end
and Direct_abs_declarator : S_direct_abs_declarator
  with type dec  := Abs_declarator.t
   and type par  := Param_type_list.t
   and type expr := Expr.t = struct
  type t =
    | Bracket of Abs_declarator.t
    | Array of t option * Expr.t option
    | Fun_decl of t option * Param_type_list.t option
  [@@deriving sexp]
  ;;
end
and Abs_declarator
  : S_abs_declarator with type ddec := Direct_abs_declarator.t = struct
  type t =
    | Pointer of Pointer.t
    | Direct of Pointer.t option * Direct_abs_declarator.t
  [@@deriving sexp]
  ;;
end

module Initialiser = struct
  type t =
    | Assign of Expr.t
    | List of t list
  [@@deriving sexp]
  ;;
end

module Init_declarator = struct
  type t =
    { declarator : Declarator.t
    ; initialiser : Initialiser.t option
    }
  [@@deriving sexp]
  ;;
end

module Decl = Make_decl (struct
    type qual = [ Storage_class_spec.t | Type_spec.t | Type_qual.t ] [@@deriving sexp]
    type decl = Init_declarator.t list [@@deriving sexp]
  end)
;;

module Label = struct
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
end

module type S_compound_stm = sig
  type stm

  (* C99 style *)
  type t = [`Stm of stm | `Decl of Decl.t] list [@@deriving sexp]
end

module rec Stm : S_stm
  with type com := Compound_stm.t = struct
  type t =
    | Label of Label.t * t
    | Expr of Expr.t option
    | Compound of Compound_stm.t
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
and Compound_stm : S_compound_stm
  with type stm := Stm.t = struct
  type t = [`Stm of Stm.t | `Decl of Decl.t] list [@@deriving sexp]
end

module Function_def = struct
  type t =
    { decl_specs : [ Storage_class_spec.t | Type_spec.t | Type_qual.t ] list
    ; signature  : Declarator.t
    ; decls      : Decl.t list
    ; body       : Compound_stm.t
    }
  [@@deriving sexp]
  ;;
end

module External_decl = struct
  type t =
    [ `Fun of Function_def.t
    | `Decl of Decl.t
    ]
  [@@deriving sexp]
  ;;
end

module Translation_unit = struct
  type t = External_decl.t list
  [@@deriving sexp]
  ;;
end

module Litmus_lang : Litmus.Ast.Basic
  with type Statement.t = [`Stm of Stm.t | `Decl of Decl.t]
   and type Program.t = Function_def.t
   and type Constant.t = Constant.t = (struct
    let name = "C"

    module Constant = struct
      include Constant
      let pp _ _ = failwith "unimplemented"
    end

    module Statement = struct
      type t = [`Stm of Stm.t | `Decl of Decl.t] [@@deriving sexp]
      let pp _ _ = failwith "unimplemented"
    end

    module Program = struct
      include Function_def

      let name x = Some (Declarator.identifier x.signature)

      let pp _ _ = failwith "unimplemented"

      let listing x = x.body
    end
  end)


module Litmus : Litmus.Ast.S
  with module Lang := Litmus_lang = Litmus.Ast.Make (Litmus_lang)
