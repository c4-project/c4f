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
open Import

let pp_assign_rhs (type a) (pp : a Fmt.t) : a Fmt.t = Fmt.(any "@ =@ " ++ pp)

let pp_opt_assign (type l r) (ppl : l Fmt.t) (ppr : r Fmt.t) :
    (l * r option) Fmt.t =
  Fmt.(pair ~sep:nop ppl (option (pp_assign_rhs ppr)))

module Optional (N : Ast_basic_types.Ast_node) :
  Ast_basic_types.Ast_node with type t = N.t option = struct
  type t = N.t option [@@deriving sexp, equal, compare]

  let pp = Fmt.option N.pp
end

module type Sep = sig
  val sep : unit Fmt.t
end

module Comma : Sep = struct
  let sep = Fmt.comma
end

module Space : Sep = struct
  let sep = Fmt.sp
end

module List_of (N : Ast_basic_types.Ast_node) (S : Sep) :
  Ast_basic_types.Ast_node with type t = N.t list = struct
  type t = N.t list [@@deriving sexp, equal, compare]

  let pp = Fmt.list ~sep:S.sep N.pp
end

module End_semi (N : Ast_basic_types.Ast_node) :
  Ast_basic_types.Ast_node with type t = N.t = struct
  type t = N.t [@@deriving sexp, equal, compare]

  let pp = Fmt.(N.pp ++ any ";")
end

(** AST nodes parametrised to break dependency cycles.

    Since the C AST is inherently mutually recursive, we build the recursive
    parts of the AST first as a set of 'parametric' functors that depend
    loosely and abstractly on on other bits of AST. We then instantiate the
    actual AST as a recursive module specification. *)
module Parametric = struct
  (** Generic declarations parametrised on qualifiers and declarator. *)
  module G_decl = struct
    module type S = Ast_types.S_g_decl

    module type Basic = sig
      (** Type of qualifiers. *)
      module Qual : Ast_basic_types.Ast_node

      (** Type of declarators. *)
      module Decl : Ast_basic_types.Ast_node
    end

    module Make (B : Basic) :
      S with type qual := B.Qual.t and type decl := B.Decl.t = struct
      type t = {qualifiers: B.Qual.t list; declarator: B.Decl.t}
      [@@deriving sexp, equal, compare]

      let pp : t Fmt.t =
        Fmt.(
          using
            (fun {qualifiers; declarator} -> (qualifiers, declarator))
            (hvbox
               (pair ~sep:sp (box (list ~sep:sp B.Qual.pp)) (box B.Decl.pp)) ))
    end
  end

  (** Generic composite specs parametrised on sort and declaration. *)
  module Composite_spec = struct
    module type S = Ast_types.S_composite_spec

    module type Basic = sig
      module Kind : Ast_basic_types.Ast_node

      module Decl : Ast_basic_types.Ast_node
    end

    module Make (B : Basic) :
      S with type kind := B.Kind.t and type decl := B.Decl.t = struct
      type t =
        | Literal of
            { kind: B.Kind.t
            ; name_opt: Ast_basic.Identifier.t option
            ; decls: B.Decl.t list }
        | Named of B.Kind.t * Ast_basic.Identifier.t
      [@@deriving sexp, equal, compare]

      let pp f = function
        | Literal {kind; name_opt; decls} ->
            Fmt.(
              pf f "%a@ %a@ %a" B.Kind.pp kind
                (option Ast_basic.Identifier.pp)
                name_opt
                (Utils.My_format.pp_c_braces (list ~sep:sp B.Decl.pp))
                decls)
        | Named (kind, id) ->
            Fmt.pf f "%a@ %a" B.Kind.pp kind Ast_basic.Identifier.pp id
    end
  end

  (** Direct declarators *)
  module Direct_declarator = struct
    module type S = Ast_types.S_direct_declarator

    module type Basic = sig
      module Dec : Ast_basic_types.Ast_node_with_identifier

      module Par : Ast_basic_types.Ast_node

      module Expr : Ast_basic_types.Ast_node
    end

    module Make (B : Basic) :
      S
        with type dec := B.Dec.t
         and type par := B.Par.t
         and type expr := B.Expr.t = struct
      type t =
        | Id of Ast_basic.Identifier.t
        | Bracket of B.Dec.t
        | Array of (t, B.Expr.t option) Ast_basic.Array.t
        | Fun_decl of t * B.Par.t
        | Fun_call of t * Ast_basic.Identifier.t list
      [@@deriving sexp, equal, compare]

      let rec pp f : t -> unit = function
        | Id i -> Ast_basic.Identifier.pp f i
        | Bracket t -> Fmt.parens B.Dec.pp f t
        | Array a -> Ast_basic.Array.pp pp (Fmt.option B.Expr.pp) f a
        | Fun_decl (t, ps) ->
            Fmt.(pair ~sep:nop pp (parens B.Par.pp) f (t, ps))
        | Fun_call (t, ps) ->
            Fmt.(
              pair ~sep:nop pp
                (parens (list ~sep:comma Ast_basic.Identifier.pp))
                f (t, ps))

      let rec identifier = function
        | Id x -> x
        | Bracket d -> B.Dec.identifier d
        | Array {array; _} -> identifier array
        | Fun_decl (t, _) -> identifier t
        | Fun_call (t, _) -> identifier t
    end
  end

  (** Declarators *)
  module Declarator = struct
    module type S = Ast_types.S_declarator

    module Make (D : Ast_basic_types.Ast_node_with_identifier) :
      S with type ddec := D.t = struct
      type t = {pointer: Ast_basic.Pointer.t option; direct: D.t}
      [@@deriving sexp, equal, compare]

      let identifier {direct; _} = D.identifier direct

      let pp =
        Fmt.(
          using
            (fun {pointer; direct} -> (pointer, direct))
            (pair ~sep:nop (option Ast_basic.Pointer.pp) D.pp))
    end
  end

  (** Direct abstract declarators *)
  module Direct_abs_declarator = struct
    module type S = Ast_types.S_direct_abs_declarator

    (* TODO(@MattWindsor91): merge this with the one from Direct_declarator
       somehow? *)

    module type Basic = sig
      module Dec : Ast_basic_types.Ast_node

      module Par : Ast_basic_types.Ast_node

      module Expr : Ast_basic_types.Ast_node
    end

    module Make (B : Basic) :
      S
        with type dec := B.Dec.t
         and type par := B.Par.t
         and type expr := B.Expr.t = struct
      type t =
        | Bracket of B.Dec.t
        | Array of (t option, B.Expr.t option) Ast_basic.Array.t
        | Fun_decl of t option * B.Par.t option
      [@@deriving sexp, equal, compare]

      let rec pp f : t -> unit = function
        | Bracket t -> Fmt.parens B.Dec.pp f t
        | Array a ->
            Fmt.(Ast_basic.Array.pp (option pp) (option B.Expr.pp) f a)
        | Fun_decl (t, ps) ->
            Fmt.(
              pair ~sep:nop (option pp) (parens (option B.Par.pp)) f (t, ps))
    end
  end

  (** Abstract declarators *)
  module Abs_declarator = struct
    module type S = Ast_types.S_abs_declarator

    module Make (D : Ast_basic_types.Ast_node) : S with type ddec := D.t =
    struct
      type t =
        | Pointer of Ast_basic.Pointer.t
        | Direct of Ast_basic.Pointer.t option * D.t
      [@@deriving sexp, equal, compare]

      let pp f : t -> unit = function
        | Pointer ptr -> Ast_basic.Pointer.pp f ptr
        | Direct (mptr, direct) ->
            Fmt.(pair ~sep:nop (option Ast_basic.Pointer.pp) D.pp)
              f (mptr, direct)
    end
  end

  (** Struct declarators *)
  module Struct_declarator = struct
    module type S = Ast_types.S_struct_declarator

    module type Basic = sig
      module Dec : Ast_basic_types.Ast_node

      module Expr : Ast_basic_types.Ast_node
    end

    module Make (B : Basic) :
      S with type dec := B.Dec.t and type expr := B.Expr.t = struct
      type t = Regular of B.Dec.t | Bitfield of B.Dec.t option * B.Expr.t
      [@@deriving sexp, equal, compare]

      let pp f : t -> unit = function
        | Regular decl -> B.Dec.pp f decl
        | Bitfield (mdecl, bitsize) ->
            Fmt.(
              pair ~sep:nop
                (* Trying to get 'X : Y' if X exists, and ': Y' if not. *)
                (option (B.Dec.pp ++ sp))
                (any ":@ " ++ B.Expr.pp)
                f (mdecl, bitsize))
    end
  end

  module Label = struct
    module type S = Ast_types.S_label

    module Make (E : Ast_basic_types.Ast_node) : S with type expr := E.t =
    struct
      type t = Normal of Ast_basic.Identifier.t | Case of E.t | Default
      [@@deriving sexp, equal, compare]

      let pp_body (f : Base.Formatter.t) : t -> unit = function
        | Normal id -> Ast_basic.Identifier.pp f id
        | Case expr -> Fmt.pf f "case@ %a" E.pp expr
        | Default -> Fmt.string f "default"

      let pp : t Fmt.t = Fmt.(pp_body ++ any ":")
    end
  end

  module Expr = struct
    module type S = Ast_types.S_expr

    module Make (T : Ast_basic_types.Ast_node) : S with module Ty = T =
    struct
      module Ty = T

      type t =
        | Prefix of Operators.Pre.t * t
        | Postfix of t * Operators.Post.t
        | Binary of t * Operators.Bin.t * t
        | Ternary of {cond: t; t_expr: t; f_expr: t}
        | Cast of T.t * t
        | Call of {func: t; arguments: t list}
        | Subscript of (t, t) Ast_basic.Array.t
        | Field of
            { value: t
            ; field: Ast_basic.Identifier.t
            ; access: [`Direct (* . *) | `Deref (* -> *)] }
        | Sizeof_type of T.t
        | Identifier of Ast_basic.Identifier.t
        | String of String.t
        | Constant of Ast_basic.Constant.t
        | Brackets of t
      [@@deriving sexp, equal, compare]

      let rec pp f : t -> unit = function
        | Prefix (pre, t) ->
            Fmt.pair ~sep:Fmt.nop Operators.Pre.pp pp f (pre, t)
        | Postfix (t, post) ->
            Fmt.pair ~sep:Fmt.nop pp Operators.Post.pp f (t, post)
        | Binary (l, bin, r) ->
            Fmt.pf f "%a@ %a@ %a" pp l Operators.Bin.pp bin pp r
        | Ternary {cond; t_expr; f_expr} ->
            Fmt.pf f "%a@ ?@ %a@ :@ %a" pp cond pp t_expr pp f_expr
        | Cast (ty, t) -> Fmt.(pf f "%a%a" (parens T.pp) ty pp t)
        | Call {func; arguments} ->
            Fmt.(pf f "%a%a" pp func (parens (list ~sep:comma pp)) arguments)
        | Subscript a -> Ast_basic.Array.pp pp pp f a
        | Field {value; field; access= `Direct} ->
            Fmt.pf f "%a.%a" pp value Ast_basic.Identifier.pp field
        | Field {value; field; access= `Deref} ->
            Fmt.pf f "%a->%a" pp value Ast_basic.Identifier.pp field
        | Sizeof_type ty -> Fmt.(pf f "sizeof%a" (parens T.pp) ty)
        | Identifier id -> Ast_basic.Identifier.pp f id
        | String s ->
            (* TODO(@MattWindsor91): escape sequences *)
            Fmt.(quote ~mark:"\"" string) f s
        | Constant k -> Ast_basic.Constant.pp f k
        | Brackets t -> Fmt.parens pp f t
    end
  end

  module Param_type_list = struct
    module type S = Ast_types.S_param_type_list

    module Make (P : Ast_basic_types.Ast_node) : S with type pdecl := P.t =
    struct
      type t = {params: P.t list; style: [`Normal | `Variadic]}
      [@@deriving sexp, equal, compare]

      let pp_style f = function
        | `Normal -> Fmt.nop f ()
        | `Variadic -> Fmt.any "@ ,@ ..." f ()

      let pp =
        Fmt.(
          using
            (fun {params; style} -> (params, style))
            (pair ~sep:nop (list ~sep:comma P.pp) pp_style))
    end
  end

  module Stm = struct
    module type S = Ast_types.S_stm

    module type Basic = sig
      module Com : Ast_basic_types.Ast_node

      module Expr : Ast_basic_types.Ast_node

      module Lbl : Ast_basic_types.Ast_node
    end

    module Make (B : Basic) :
      S
        with type com := B.Com.t
         and type expr := B.Expr.t
         and type lbl := B.Lbl.t = struct
      type t =
        | Label of B.Lbl.t * t
        | Expr of B.Expr.t option
        | Compound of B.Com.t
        | Atomic of B.Com.t
        | Synchronized of B.Com.t
        | If of {cond: B.Expr.t; t_branch: t; f_branch: t option}
        | Switch of B.Expr.t * t
        | While of B.Expr.t * t
        | Do_while of t * B.Expr.t
        | For of
            { init: B.Expr.t option
            ; cond: B.Expr.t option
            ; update: B.Expr.t option
            ; body: t }
        | Goto of Ast_basic.Identifier.t
        | Continue
        | Break
        | Return of B.Expr.t option
      [@@deriving sexp, equal, compare]

      let rec pp (f : Base.Formatter.t) : t -> unit = function
        | Label (label, labelled) ->
            Fmt.(pair ~sep:sp B.Lbl.pp pp f (label, labelled))
        | Expr e -> Fmt.((option B.Expr.pp ++ any ";") f e)
        | Compound com -> B.Com.pp f com
        | Atomic a -> Fmt.(pf f "atomic@ %a" B.Com.pp a)
        | Synchronized a -> Fmt.(pf f "synchronized@ %a" B.Com.pp a)
        | If {cond; t_branch; f_branch} ->
            Fmt.(
              pf f "if@ (%a)@ %a%a" B.Expr.pp cond pp t_branch
                (option (any "@ else@ " ++ pp))
                f_branch)
        | Switch (cond, rest) ->
            Fmt.(pf f "switch@ (%a)@ %a" B.Expr.pp cond pp rest)
        | Continue -> Fmt.any "continue;" f ()
        | Break -> Fmt.any "break;" f ()
        | While (cond, body) ->
            Fmt.(pf f "while@ (%a)@ %a" B.Expr.pp cond pp body)
        | Do_while (body, cond) ->
            Fmt.(pf f "do@ %a@ while@ (%a);" pp body B.Expr.pp cond)
        | For {init; cond; update; body} ->
            Fmt.(
              pf f "for@ (%a;@ %a;@ %a)@ %a" (option B.Expr.pp) init
                (option B.Expr.pp) cond (option B.Expr.pp) update pp body)
        | Goto label -> Fmt.pf f "goto@ %a;" Ast_basic.Identifier.pp label
        | Return expr ->
            (* The space should be surpressed when there is no return value,
               hence the somewhat odd formulation. *)
            Fmt.(pf f "return%a;" (option (sp ++ B.Expr.pp)) expr)
    end
  end

  module Compound_stm = struct
    module type S = Ast_types.S_compound_stm

    module type Basic = sig
      module Decl : Ast_basic_types.Ast_node

      module Stm : Ast_basic_types.Ast_node
    end

    module Make (B : Basic) :
      S with type decl := B.Decl.t and type stm := B.Stm.t = struct
      module Elt = struct
        type t = [`Stm of B.Stm.t | `Decl of B.Decl.t]
        [@@deriving sexp, equal, compare]

        let pp (f : Base.Formatter.t) : t -> unit = function
          | `Stm s -> B.Stm.pp f s
          | `Decl d -> B.Decl.pp f d
      end

      type t = Elt.t list [@@deriving sexp, equal, compare]

      let pp : t Fmt.t =
        Utils.My_format.pp_c_braces Fmt.(list ~sep:sp (box Elt.pp))
    end
  end
end

module rec Expr : (Ast_types.S_expr with type Ty.t = Type_name.t) =
  Parametric.Expr.Make (Type_name)

and Enumerator : sig
  type t = {name: Ast_basic.Identifier.t; value: Expr.t option}

  include Ast_basic_types.Ast_node with type t := t
end = struct
  type t = {name: Ast_basic.Identifier.t; value: Expr.t option}
  [@@deriving sexp, equal, compare]

  let pp : t Fmt.t =
    Fmt.(
      using
        (fun {name; value} -> (name, value))
        (pp_opt_assign Ast_basic.Identifier.pp Expr.pp))
end

and Enum_spec :
  (Ast_types.S_composite_spec
    with type kind := [`Enum]
     and type decl := Enumerator.t) = Parametric.Composite_spec.Make (struct
  module Kind = struct
    type t = [`Enum] [@@deriving sexp, equal, compare]

    let pp f = function `Enum -> Fmt.string f "enum"
  end

  module Decl = Enumerator
end)

and Struct_decl :
  (Ast_types.S_g_decl
    with type qual := Spec_or_qual.t
     and type decl := Struct_declarator.t list) =
Parametric.G_decl.Make (struct
  module Qual = Spec_or_qual
  module Decl = List_of (Struct_declarator) (Space)
end)

and Type_spec :
  (Ast_types.S_type_spec
    with type su := Struct_or_union_spec.t
     and type en := Enum_spec.t) = struct
  type t =
    [ Ast_basic.Prim_type.t
    | `Struct_or_union of Struct_or_union_spec.t
    | `Enum of Enum_spec.t
    | `Defined_type of Ast_basic.Identifier.t ]
  [@@deriving sexp, equal, compare]

  let pp f : t -> unit = function
    | #Ast_basic.Prim_type.t as prim -> Ast_basic.Prim_type.pp f prim
    | `Struct_or_union spec -> Struct_or_union_spec.pp f spec
    | `Enum spec -> Enum_spec.pp f spec
    | `Defined_type tdef -> Ast_basic.Identifier.pp f tdef
end

and Spec_or_qual :
  (Ast_basic_types.Ast_node
    with type t = [Type_spec.t | Ast_basic.Type_qual.t]) = struct
  type t = [Type_spec.t | Ast_basic.Type_qual.t]
  [@@deriving equal, sexp_of, compare]

  let t_of_sexp (s : Sexp.t) : t =
    try (Type_spec.t_of_sexp s :> t)
    with _ -> (Ast_basic.Type_qual.t_of_sexp s :> t)

  let pp f : t -> unit = function
    | #Type_spec.t as spec -> Type_spec.pp f spec
    | #Ast_basic.Type_qual.t as qual -> Ast_basic.Type_qual.pp f qual
end

and Decl_spec :
  (Ast_basic_types.Ast_node
    with type t =
      [Ast_basic.Storage_class_spec.t | Type_spec.t | Ast_basic.Type_qual.t]) =
struct
  type t =
    [Ast_basic.Storage_class_spec.t | Type_spec.t | Ast_basic.Type_qual.t]
  [@@deriving sexp_of, equal, compare]

  let t_of_sexp (s : Sexp.t) : t =
    try (Ast_basic.Storage_class_spec.t_of_sexp s :> t)
    with _ -> (Spec_or_qual.t_of_sexp s :> t)

  let pp f : t -> unit = function
    | #Ast_basic.Storage_class_spec.t as spec ->
        Ast_basic.Storage_class_spec.pp f spec
    | #Type_spec.t as spec -> Type_spec.pp f spec
    | #Ast_basic.Type_qual.t as qual -> Ast_basic.Type_qual.pp f qual
end

and Type_name :
  (Ast_types.S_g_decl
    with type qual := Spec_or_qual.t
     and type decl := Abs_declarator.t option) =
Parametric.G_decl.Make (struct
  module Qual = Spec_or_qual
  module Decl = Optional (Abs_declarator)
end)

and Struct_or_union_spec :
  (Ast_types.S_composite_spec
    with type kind := [`Struct | `Union]
     and type decl := Struct_decl.t) = Parametric.Composite_spec.Make (struct
  module Kind = struct
    type t = [`Struct | `Union] [@@deriving sexp, equal, compare]

    let to_string : t -> string = function
      | `Struct -> "struct"
      | `Union -> "union"

    let pp : t Fmt.t = Fmt.of_to_string to_string
  end

  module Decl = Struct_decl
end)

and Param_decl :
  (Ast_types.S_g_decl
    with type qual := Decl_spec.t
     and type decl :=
      [`Concrete of Declarator.t | `Abstract of Abs_declarator.t option]) =
Parametric.G_decl.Make (struct
  module Qual = Decl_spec

  module Decl = struct
    type t =
      [`Concrete of Declarator.t | `Abstract of Abs_declarator.t option]
    [@@deriving sexp, equal, compare]

    let pp f : t -> unit = function
      | `Concrete c -> Declarator.pp f c
      | `Abstract a -> (Fmt.option Abs_declarator.pp) f a
  end
end)

and Param_type_list :
  (Parametric.Param_type_list.S with type pdecl := Param_decl.t) =
  Parametric.Param_type_list.Make (Param_decl)

and Direct_declarator :
  (Ast_types.S_direct_declarator
    with type dec := Declarator.t
     and type par := Param_type_list.t
     and type expr := Expr.t) = Parametric.Direct_declarator.Make (struct
  module Dec = Declarator
  module Par = Param_type_list
  module Expr = Expr
end)

and Declarator :
  (Parametric.Declarator.S with type ddec := Direct_declarator.t) =
  Parametric.Declarator.Make (Direct_declarator)

and Struct_declarator :
  (Ast_types.S_struct_declarator
    with type dec := Declarator.t
     and type expr := Expr.t) = Parametric.Struct_declarator.Make (struct
  module Dec = Declarator
  module Expr = Expr
end)

and Direct_abs_declarator :
  (Ast_types.S_direct_abs_declarator
    with type dec := Abs_declarator.t
     and type par := Param_type_list.t
     and type expr := Expr.t) = Parametric.Direct_abs_declarator.Make (struct
  module Dec = Abs_declarator
  module Par = Param_type_list
  module Expr = Expr
end)

and Abs_declarator :
  (Parametric.Abs_declarator.S with type ddec := Direct_abs_declarator.t) =
  Parametric.Abs_declarator.Make (Direct_abs_declarator)

module Initialiser = struct
  type t = Assign of Expr.t | List of t list
  [@@deriving sexp, equal, compare]

  let rec pp f : t -> unit = function
    | Assign exp -> Expr.pp f exp
    | List inits -> Fmt.(braces (list ~sep:comma pp)) f inits
end

module Init_declarator = struct
  type t = {declarator: Declarator.t; initialiser: Initialiser.t option}
  [@@deriving sexp, equal, compare]

  let pp =
    Fmt.(
      using
        (fun {declarator; initialiser} -> (declarator, initialiser))
        (pp_opt_assign Declarator.pp Initialiser.pp))
end

module Decl = Parametric.G_decl.Make (struct
  module Qual = Decl_spec
  module Decl = End_semi (List_of (Init_declarator) (Comma))
end)

module Label = Parametric.Label.Make (Expr)

module rec Stm :
  (Ast_types.S_stm
    with type com := Compound_stm.t
     and type expr := Expr.t
     and type lbl := Label.t) = Parametric.Stm.Make (struct
  module Com = Compound_stm
  module Expr = Expr
  module Lbl = Label
end)

and Compound_stm :
  (Ast_types.S_compound_stm with type decl := Decl.t and type stm := Stm.t) =
Parametric.Compound_stm.Make (struct
  module Decl = Decl
  module Stm = Stm
end)

module Function_def = struct
  type t =
    { decl_specs: Decl_spec.t list
    ; signature: Declarator.t
    ; decls: Decl.t list
    ; body: Compound_stm.t }
  [@@deriving sexp, equal, compare]

  let pp_oldstyle_decl_list : Decl.t list Fmt.t =
    Fmt.(
      using
        (function [] -> None | x -> Some x)
        (option (any "@ " ++ list ~sep:sp Decl.pp)))

  let pp (f : Base.Formatter.t) {decl_specs; signature; decls; body} : unit =
    Fmt.(
      pf f "%a@ %a%a@ %a"
        (box (list ~sep:sp Decl_spec.pp))
        decl_specs Declarator.pp signature pp_oldstyle_decl_list decls
        Compound_stm.pp body)
end

module External_decl = struct
  type t = [`Fun of Function_def.t | `Decl of Decl.t]
  [@@deriving sexp, equal, compare]

  let pp (f : Base.Formatter.t) : t -> unit = function
    | `Fun fn -> Function_def.pp f fn
    | `Decl d -> Decl.pp f d
end

module Translation_unit = struct
  type t = External_decl.t list [@@deriving sexp, equal, compare]

  let pp : t Fmt.t = Fmt.(vbox (list ~sep:sp External_decl.pp))
end

module Litmus_lang :
  C4f_litmus.Test_types.Basic
    with type Statement.t = [`Stm of Stm.t | `Decl of Decl.t]
     and type Program.t = Function_def.t
     and type Constant.t = Ast_basic.Constant.t = struct
  let name = "C"

  module Constant = Ast_basic.Constant

  module Statement = struct
    include Compound_stm.Elt

    let empty () = `Stm (Stm.Expr None)

    let make_uniform = Tx.List.right_pad ~padding:(empty ())
  end

  module Type = Type_spec

  module Program = struct
    include Function_def

    let name x =
      Some
        (Ast_basic.Identifier.to_string (Declarator.identifier x.signature))

    (* TODO(@MattWindsor91): consider implementing this. The main reason why
       I haven't is because, usually, we'll be converting the litmus test to
       Mini, and that has a working global variables extraction. *)
    let global_vars = Fn.const None

    let listing x = x.body
  end
end

module Litmus = struct
  module A = C4f_litmus.Test.Make (Litmus_lang)
  include A
  module Id = C4f_common.Litmus_id
end

module LP = C4f_litmus.Predicate

module P = struct
  type t = Litmus_lang.Constant.t LP.t
  [@@deriving sexp, quickcheck, compare, equal]
end
