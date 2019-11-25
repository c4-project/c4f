(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Ast = Act_c_lang.Ast

let to_initialiser (value : Constant.t) : Ast.Initialiser.t =
  Assign (Reify_expr.constant value)

let type_to_spec (ty : Type.t) : [> Act_c_lang.Ast.Type_spec.t] =
  (* We translate the level of indirection separately, in [type_to_pointer]. *)
  Type.Basic.to_spec (Type.basic_type ty)

let type_to_pointer (ty : Type.t) : Act_c_lang.Ast_basic.Pointer.t option =
  (* We translate the actual underlying type separately, in [type_to_spec]. *)
  Option.some_if (Type.is_pointer ty) [[]]

let id_declarator (ty : Type.t) (id : Act_common.C_id.t) : Ast.Declarator.t =
  {pointer= type_to_pointer ty; direct= Id id}

let decl (id : Act_common.C_id.t) (elt : Initialiser.t) : Ast.Decl.t =
  let ty = Initialiser.ty elt in
  let value = Initialiser.value elt in
  { qualifiers= [type_to_spec ty]
  ; declarator=
      [ { declarator= id_declarator ty id
        ; initialiser= Option.map ~f:to_initialiser value } ] }

let decls : Initialiser.t Named.Alist.t -> [> `Decl of Ast.Decl.t] list =
  List.map ~f:(fun (k, v) -> `Decl (decl k v))

let func_parameter (id : Act_common.C_id.t) (ty : Type.t) : Ast.Param_decl.t
    =
  {qualifiers= [type_to_spec ty]; declarator= `Concrete (id_declarator ty id)}

let func_parameters (parameters : Type.t Named.Alist.t) :
    Ast.Param_type_list.t =
  { params= List.map ~f:(Tuple2.uncurry func_parameter) parameters
  ; style= `Normal }

let func_signature (id : Act_common.C_id.t)
    (parameters : Type.t Named.Alist.t) : Ast.Declarator.t =
  {pointer= None; direct= Fun_decl (Id id, func_parameters parameters)}

let func_body (ds : Initialiser.t Named.Alist.t)
    (statements : _ Statement.t list) : Ast.Compound_stm.t =
  decls ds @ Reify_stm.reify_compound statements

let func (id : Act_common.C_id.t) (def : _ Function.t) : Ast.External_decl.t
    =
  let parameters = Function.parameters def in
  let body_decls = Function.body_decls def in
  let body_stms = Function.body_stms def in
  `Fun
    { decl_specs= [`Void]
    ; signature= func_signature id parameters
    ; decls= []
    ; body= func_body body_decls body_stms }

let program (prog : _ Program.t) : Ast.Translation_unit.t =
  let globals = Program.globals prog in
  let functions = Program.functions prog in
  List.concat [decls globals; List.map ~f:(Tuple2.uncurry func) functions]
