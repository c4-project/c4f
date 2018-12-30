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
open Ast_basic

include Ast_intf


let pp_assign_rhs (pp : 'a Fmt.t) : 'a Fmt.t = Fmt.(prefix (unit "@ =@ ") pp)


let pp_opt_assign (ppl : 'l Fmt.t) (ppr : 'r Fmt.t)
  : ('l * 'r option) Fmt.t =
  Fmt.(append ppl (option (pp_assign_rhs ppr)))
;;

module Optional (N : Ast_node)
  : Ast_node with type t = N.t option = struct
  type t = N.t option [@@deriving sexp]
  let pp = Fmt.option N.pp
end

module type Sep = sig
  val sep : unit Fmt.t
end

module Comma : Sep = struct let sep = Fmt.comma end
module Space : Sep = struct let sep = Fmt.sp end

module List_of (N : Ast_node) (S : Sep)
  : Ast_node with type t = N.t list = struct
  type t = N.t list [@@deriving sexp]
  let pp = Fmt.list ~sep:S.sep N.pp
end

(** AST nodes parametrised to break dependency cycles.

    Since the C AST is inherently mutually recursive, we build the
   recursive parts of the AST first as a set of 'parametric' functors
   that depend loosely and abstractly on on other bits of AST.  We
   then instantiate the actual AST as a recursive module
   specification. *)
module Parametric = struct
  (** Generic declarations parametrised on qualifiers and declarator. *)
  module G_decl = struct
    module type S = S_g_decl

    module type Basic = sig
      module Qual : Ast_node (** Type of qualifiers *)
      module Decl : Ast_node (** Type of declarators *)
    end

    module Make (B : Basic) : S
      with type qual := B.Qual.t
       and type decl := B.Decl.t = struct
      type t =
        { qualifiers : B.Qual.t list
        ; declarator : B.Decl.t
        }
      [@@deriving sexp]
      ;;

      let pp : t Fmt.t =
        Fmt.(
          using
            (fun { qualifiers; declarator } -> (qualifiers, declarator))
            (pair ~sep:sp
               (list ~sep:sp B.Qual.pp)
               B.Decl.pp
            )
        )
    end
  end

  (** Generic composite specs parametrised on sort and declaration. *)
  module Composite_spec = struct
    module type S = S_composite_spec

    module type Basic = sig
      module Kind : Ast_node
      module Decl : Ast_node
    end

    module Make (B : Basic)
      : S with type kind := B.Kind.t
           and type decl := B.Decl.t = struct
      type t =
        | Literal of
            { kind     : B.Kind.t
            ; name_opt : Identifier.t option
            ; decls    : B.Decl.t list
            }
        | Named of B.Kind.t * Identifier.t
      [@@deriving sexp]

      let pp f = function
        | Literal { kind; name_opt; decls } ->
          Fmt.(
            pf f "%a@ %a@ %a"
              B.Kind.pp kind
              (option string) name_opt
              (Utils.My_format.pp_c_braces (list ~sep:sp B.Decl.pp)) decls
          )
        | Named (kind, id) ->
          Fmt.pf f "%a@ %s" B.Kind.pp kind id
      ;;
    end
  end

  (** Direct declarators *)
  module Direct_declarator = struct
    module type S = S_direct_declarator

    module type Basic = sig
      module Dec  : Ast_node_with_identifier
      module Par  : Ast_node
      module Expr : Ast_node
    end

    module Make (B : Basic)
      : S with type dec  := B.Dec.t
           and type par  := B.Par.t
           and type expr := B.Expr.t = struct
      type t =
        | Id of Identifier.t
        | Bracket of B.Dec.t
        | Array of (t, B.Expr.t option) Array.t
        | Fun_decl of t * B.Par.t
        | Fun_call of t * Identifier.t list
      [@@deriving sexp]

      let rec pp f : t -> unit = function
        | Id      i  -> Fmt.string f i
        | Bracket t  -> Fmt.brackets B.Dec.pp f t
        | Array   a  -> Array.pp pp (Fmt.option B.Expr.pp) f a
        | Fun_decl (t, ps) ->
          Fmt.(append pp (parens B.Par.pp) f (t, ps))
        | Fun_call (t, ps) ->
          Fmt.(append pp (parens (list ~sep:comma Identifier.pp)) f (t, ps))
      ;;

      let rec identifier = function
        | Id x -> x
        | Bracket d -> B.Dec.identifier d
        | Array { array;  _ } -> identifier array
        | Fun_decl (t, _) -> identifier t
        | Fun_call (t, _) -> identifier t
      ;;
    end
  end

  (** Declarators *)
  module Declarator = struct
    module type S = S_declarator

    module Make (D : Ast_node_with_identifier)
      : S with type ddec := D.t = struct
      type t =
        { pointer : Pointer.t option
        ; direct  : D.t
        }
      [@@deriving sexp]
      ;;

      let identifier { direct; _ } = D.identifier direct

      let pp =
        Fmt.(
          using (fun { pointer; direct } -> (pointer, direct))
            (append (option Pointer.pp) D.pp)
        )
      ;;
    end
  end

  (** Direct abstract declarators *)
  module Direct_abs_declarator = struct
    module type S = S_direct_abs_declarator

    (* TODO(@MattWindsor91): merge this with the one from Direct_declarator somehow? *)

    module type Basic = sig
      module Dec  : Ast_node
      module Par  : Ast_node
      module Expr : Ast_node
    end

    module Make (B : Basic)
      : S with type dec  := B.Dec.t
           and type par  := B.Par.t
           and type expr := B.Expr.t = struct
      type t =
        | Bracket of B.Dec.t
        | Array of (t option, B.Expr.t option) Array.t
        | Fun_decl of t option * B.Par.t option
      [@@deriving sexp]
      ;;

      let rec pp f : t -> unit = function
        | Bracket t  -> Fmt.brackets B.Dec.pp f t
        | Array   a  -> Fmt.(Array.pp (option pp) (option B.Expr.pp) f a)
        | Fun_decl (t, ps) ->
          Fmt.(append (option pp) (parens (option B.Par.pp)) f (t, ps))
      ;;
    end
  end

  (** Abstract declarators *)
  module Abs_declarator = struct
    module type S = S_abs_declarator

    module Make (D : Ast_node) : S with type ddec := D.t = struct
      type t =
        | Pointer of Pointer.t
        | Direct of Pointer.t option * D.t
      [@@deriving sexp]
      ;;

      let pp f : t -> unit = function
        | Pointer ptr -> Pointer.pp f ptr
        | Direct (mptr, direct) ->
          Fmt.((append (option Pointer.pp) D.pp)) f (mptr, direct)
    end
  end

  (** Struct declarators *)
  module Struct_declarator = struct
    module type S = S_struct_declarator

    module type Basic = sig
      module Dec  : Ast_node
      module Expr : Ast_node
    end

    module Make (B : Basic)
      : S with type dec  := B.Dec.t
           and type expr := B.Expr.t = struct
      type t =
        | Regular of B.Dec.t
        | Bitfield of B.Dec.t option * B.Expr.t
      [@@deriving sexp]
      ;;

      let pp f : t -> unit = function
        | Regular decl -> B.Dec.pp f decl
        | Bitfield (mdecl, bitsize) ->
          Fmt.(
            append
              (* Trying to get 'X : Y' if X exists, and ': Y' if not. *)
              (option (suffix sp B.Dec.pp))
              (prefix (unit ":@ ") B.Expr.pp)
              f (mdecl, bitsize)
          )
      ;;
    end
  end

  module Label = struct
    module type S = S_label

    module Make (E : Ast_node) : S with type expr := E.t = struct
      type t =
        | Normal of Identifier.t
        | Case   of E.t
        | Default
      [@@deriving sexp]
      ;;

      let pp_body (f : Base.Formatter.t) : t -> unit = function
        | Normal id -> Identifier.pp f id
        | Case expr -> Fmt.pf f "case@ %a" E.pp expr
        | Default   -> Fmt.string f "default"
      ;;

      let pp : t Fmt.t = Fmt.(suffix (unit ":") pp_body)
    end
  end

  module Expr = struct
    module type S = S_expr
    module Make (T : Ast_node) : S with module Ty := T = struct
      type t =
        | Prefix      of Operators.Pre.t * t
        | Postfix     of t * Operators.Post.t
        | Binary      of t * Operators.Bin.t * t
        | Ternary     of { cond   : t
                         ; t_expr : t
                         ; f_expr : t
                         }
        | Cast        of T.t * t
        | Call        of { func : t; arguments : t list}
        | Subscript   of (t, t) Array.t
        | Field       of { value  : t
                         ; field  : Identifier.t
                         ; access : [ `Direct (* . *) | `Deref (* -> *) ]
                         }
        | Sizeof_type of T.t
        | Identifier  of Identifier.t
        | String      of String.t
        | Constant    of Constant.t
        | Brackets    of t
      [@@deriving sexp]
      ;;

      let rec pp f : t -> unit = function
        | Prefix (pre, t) -> Fmt.append Operators.Pre.pp pp f (pre, t)
        | Postfix (t, post) -> Fmt.append pp Operators.Post.pp f (t, post)
        | Binary (l, bin, r) ->
          Fmt.pf f "%a@ %a@ %a"
            pp l
            Operators.Bin.pp bin
            pp r
        | Ternary { cond; t_expr; f_expr } ->
          Fmt.pf f "%a@ ?@ %a@ :@ %a"
            pp cond
            pp t_expr
            pp f_expr
        | Cast (ty, t) ->
          Fmt.(
            pf f "%a%a"
              (parens T.pp) ty
              pp t
          )
        | Call { func; arguments } ->
          Fmt.(
            pf f "%a%a"
              pp func
              (parens (list ~sep:comma pp)) arguments
          )
        | Subscript a -> Array.pp pp pp f a
        | Field { value; field; access = `Direct } ->
          Fmt.pf f "%a.%s" pp value field
        | Field { value; field; access = `Deref } ->
          Fmt.pf f "%a->%s" pp value field
        | Sizeof_type ty ->
          Fmt.(pf f "sizeof%a" (parens T.pp) ty)
        | Identifier id -> Fmt.string f id
        | String s -> (* TODO(@MattWindsor91): escape sequences *)
          Fmt.(quote ~mark:"\"" string) f s
        | Constant k -> Constant.pp f k
        | Brackets t -> Fmt.brackets pp f t
      ;;
    end
  end
end

module rec Expr : S_expr with module Ty := Type_name =
  Parametric.Expr.Make (Type_name)
and Enumerator : sig
  type t =
    { name  : Identifier.t
    ; value : Expr.t option
    }
  [@@deriving sexp]
  ;;

  include Ast_node with type t := t
end = struct
  type t =
    { name  : Identifier.t
    ; value : Expr.t option
    }
  [@@deriving sexp]
  ;;

  let pp =
    Fmt.(
      using (fun { name; value } -> (name, value))
        (pp_opt_assign Fmt.string Expr.pp)
    )
  ;;
end
and Enum_spec
  : Parametric.Composite_spec.S
    with type kind := [`Enum]
     and type decl := Enumerator.t =
  Parametric.Composite_spec.Make (struct
    module Kind = struct
      type t = [`Enum] [@@deriving sexp]
      let pp f = function `Enum -> Fmt.string f "enum"
    end
    module Decl = Enumerator
  end)
and Struct_decl
  : Parametric.G_decl.S
    with type qual := Spec_or_qual.t
     and type decl := Struct_declarator.t list =
  Parametric.G_decl.Make (struct
    module Qual = Spec_or_qual
    module Decl = List_of (Struct_declarator) (Space)
  end)
and Type_spec : S_type_spec
  with type su := Struct_or_union_spec.t
   and type en := Enum_spec.t = struct
  type t =
    [ Prim_type.t
    | `Struct_or_union of Struct_or_union_spec.t
    | `Enum of Enum_spec.t
    | `Defined_type of Identifier.t
    ]
  [@@deriving sexp]
  ;;

  let pp f : t -> unit = function
    | #Prim_type.t as prim -> Prim_type.pp f prim
    | `Struct_or_union spec -> Struct_or_union_spec.pp f spec
    | `Enum            spec -> Enum_spec.pp            f spec
    | `Defined_type    tdef -> Fmt.string              f tdef
  ;;
end
and Spec_or_qual : Ast_node
  with type t = [ Type_spec.t | Type_qual.t ] = struct
  type t = [ Type_spec.t | Type_qual.t ] [@@deriving sexp]

  let pp f : t -> unit = function
    | #Type_spec.t as spec -> Type_spec.pp f spec
    | #Type_qual.t as qual -> Type_qual.pp f qual
  ;;
end
and Decl_spec : Ast_node
  with type t = [ Storage_class_spec.t | Type_spec.t | Type_qual.t ] = struct
  type t = [ Storage_class_spec.t | Type_spec.t | Type_qual.t ] [@@deriving sexp]

  let pp f : t -> unit = function
    | #Storage_class_spec.t as spec -> Storage_class_spec.pp f spec
    | #Type_spec.t          as spec -> Type_spec.pp          f spec
    | #Type_qual.t          as qual -> Type_qual.pp          f qual
  ;;
end
and Type_name
  : Parametric.G_decl.S
    with type qual := Spec_or_qual.t
     and type decl := Abs_declarator.t option =
  Parametric.G_decl.Make (struct
    module Qual = Spec_or_qual
    module Decl = Optional (Abs_declarator)
  end)
and Struct_or_union_spec
  : Parametric.Composite_spec.S
    with type kind := [`Struct | `Union]
     and type decl := Struct_decl.t =
  Parametric.Composite_spec.Make (struct
    module Kind = struct
      type t  = [`Struct | `Union] [@@deriving sexp]

      let to_string : t -> string = function
        | `Struct -> "struct"
        | `Union  -> "union"
      ;;

      let pp : t Fmt.t = Fmt.of_to_string to_string
    end

    module Decl = Struct_decl
end)
and Param_decl
  : Parametric.G_decl.S
    with type qual := Decl_spec.t
     and type decl := [ `Concrete of Declarator.t
                      | `Abstract of Abs_declarator.t option
                      ] =
  Parametric.G_decl.Make (struct
    module Qual = Decl_spec
    module Decl = struct
      type t = [ `Concrete of Declarator.t
               | `Abstract of Abs_declarator.t option
               ] [@@deriving sexp]
      ;;
      let pp f : t -> unit = function
        | `Concrete c -> Declarator.pp                  f c
        | `Abstract a -> (Fmt.option Abs_declarator.pp) f a
      ;;
    end
  end)
and Param_type_list : S_param_type_list with type pdecl := Param_decl.t = struct
  type t =
    { params : Param_decl.t list
    ; style  : [`Normal | `Variadic]
    }
  [@@deriving sexp]
  ;;

  let pp_style f = function
    | `Normal -> Fmt.nop f ()
    | `Variadic -> Fmt.unit "@ ,@ ..." f ()
  ;;

  let pp =
    Fmt.(
      using (fun { params; style } -> (params, style))
        (append (list ~sep:comma Param_decl.pp) pp_style)
    )
  ;;
end
and Direct_declarator
  : Parametric.Direct_declarator.S with type dec  := Declarator.t
                                    and type par  := Param_type_list.t
                                    and type expr := Expr.t =
  Parametric.Direct_declarator.Make (struct
    module Dec = Declarator
    module Par = Param_type_list
    module Expr = Expr
  end)
and Declarator
  : Parametric.Declarator.S with type ddec := Direct_declarator.t =
  Parametric.Declarator.Make (Direct_declarator)
and Struct_declarator
  : Parametric.Struct_declarator.S
    with type dec := Declarator.t
     and type expr := Expr.t =
  Parametric.Struct_declarator.Make (struct
    module Dec  = Declarator
    module Expr = Expr
  end)
and Direct_abs_declarator
  : Parametric.Direct_abs_declarator.S
    with type dec  := Abs_declarator.t
     and type par  := Param_type_list.t
     and type expr := Expr.t =
  Parametric.Direct_abs_declarator.Make (struct
    module Dec = Abs_declarator
    module Par = Param_type_list
    module Expr = Expr
  end)
and Abs_declarator
  : Parametric.Abs_declarator.S
    with type ddec := Direct_abs_declarator.t =
  Parametric.Abs_declarator.Make (Direct_abs_declarator)
;;

module Initialiser = struct
  type t =
    | Assign of Expr.t
    | List of t list
  [@@deriving sexp]
  ;;

  let rec pp f : t -> unit = function
    | Assign exp   -> Expr.pp f exp
    | List   inits -> Fmt.(braces (list ~sep:comma pp)) f inits
  ;;
end

module Init_declarator = struct
  type t =
    { declarator : Declarator.t
    ; initialiser : Initialiser.t option
    }
  [@@deriving sexp]
  ;;

  let pp =
    Fmt.(
      using (fun { declarator; initialiser } -> (declarator, initialiser))
        (pp_opt_assign Declarator.pp Initialiser.pp)
    )
  ;;
end

module Decl = Parametric.G_decl.Make (struct
    module Qual = Decl_spec
    module Decl = List_of (Init_declarator) (Space)
  end)
;;

module Label = Parametric.Label.Make (Expr)

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


module Litmus : Litmus.Ast.S with module Lang := Litmus_lang =
  Litmus.Ast.Make (Litmus_lang)
;;
