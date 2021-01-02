(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020, 2021 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let bool_lit (b : bool) : Ast.Expr.t =
  Ast.Expr.Identifier
    (Act_common.C_id.of_string (if b then "true" else "false"))

let constant : Fir.Constant.t -> Ast.Expr.t =
  Fir.Constant.reduce
    ~int:(fun i -> Ast.Expr.Constant (Integer i))
    ~bool:bool_lit

let lvalue : Fir.Lvalue.t -> Ast.Expr.t =
  Fir.Lvalue.reduce
    ~variable:(fun x -> Ast.Expr.Identifier x)
    ~deref:(fun l -> Prefix (`Deref, l))

let address : Fir.Address.t -> Ast.Expr.t =
  Fir.Address.reduce ~lvalue ~ref:(fun l -> Prefix (`Ref, l))

let to_initialiser (value : Fir.Constant.t) : Ast.Initialiser.t =
  Assign (constant value)

let basic_type_to_spec (b : Fir.Type.Basic.t) : [> Ast.Type_spec.t] =
  match Fir.Type.Basic.(prim b, is_atomic b) with
  | Int, false ->
      `Int
  | Int, true ->
      `Defined_type (Common.C_id.of_string "atomic_int")
  | Bool, false ->
      `Defined_type (Common.C_id.of_string "bool")
  | Bool, true ->
      `Defined_type (Common.C_id.of_string "atomic_bool")

let type_to_specs (ty : Fir.Type.t) : [> Ast.Decl_spec.t] list =
  (* We translate the level of indirection separately, in [type_to_pointer]. *)
  List.filter_opt
    [ Some (basic_type_to_spec (Fir.Type.basic_type ty))
    ; Option.some_if (Fir.Type.is_volatile ty) `Volatile ]

let type_to_pointer (ty : Fir.Type.t) : Ast_basic.Pointer.t option =
  (* We translate the actual underlying type separately, in [type_to_specs]. *)
  Option.some_if (Fir.Type.is_pointer ty) [[]]

let id_declarator (ty : Fir.Type.t) (id : Common.C_id.t) : Ast.Declarator.t =
  {pointer= type_to_pointer ty; direct= Id id}

let func_parameter (ty : Fir.Type.t) (id : Act_common.C_id.t) :
    Ast.Param_decl.t =
  {qualifiers= type_to_specs ty; declarator= `Concrete (id_declarator ty id)}

let decl (init : Fir.Initialiser.t Common.C_named.t) : Ast.Decl.t =
  let id = Accessor.get Common.C_named.name init in
  let elt = Accessor.get Common.C_named.value init in
  let ty = Accessor.get Fir.Initialiser.ty elt in
  let value = Accessor.get Fir.Initialiser.value elt in
  { qualifiers= type_to_specs ty
  ; declarator=
      [ { declarator= id_declarator ty id
        ; initialiser= Some (to_initialiser value) } ] }

let pp_constant : Fir.Constant.t Fmt.t = Fmt.using constant Ast.Expr.pp
