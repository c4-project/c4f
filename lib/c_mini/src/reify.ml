(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Ast = Act_c_lang.Ast
  module Named = Act_common.C_named
end

let decls : Initialiser.t Named.Alist.t -> [> `Decl of Ast.Decl.t] list =
  Fn.compose
    (List.map ~f:(fun d -> `Decl (Reify_prim.decl d)))
    Named.list_of_alist

let func_parameter (id : Act_common.C_id.t) (ty : Type.t) : Ast.Param_decl.t
    =
  { qualifiers= Reify_prim.type_to_specs ty
  ; declarator= `Concrete (Reify_prim.id_declarator ty id) }

let func_parameters (parameters : Type.t Named.Alist.t) :
    Ast.Param_type_list.t =
  { params= List.map ~f:(fun (i, t) -> func_parameter i t) parameters
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
  List.concat [decls globals; List.map ~f:(fun (i, p) -> func i p) functions]
