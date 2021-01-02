(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Fir = Act_fir
  module Named = Act_common.C_named
end

let decls : Fir.Initialiser.t Named.Alist.t -> [> `Decl of Ast.Decl.t] list =
  Fn.compose
    (List.map ~f:(fun d -> `Decl (Reify_prim.decl d)))
    Named.list_of_alist

let func_parameters (parameters : Fir.Type.t Named.Alist.t) :
    Ast.Param_type_list.t =
  { params=
      List.map ~f:(fun (i, t) -> Reify_prim.func_parameter t i) parameters
  ; style= `Normal }

let func_signature (id : Act_common.C_id.t)
    (parameters : Fir.Type.t Named.Alist.t) : Ast.Declarator.t =
  {pointer= None; direct= Fun_decl (Id id, func_parameters parameters)}

let func_body (ds : Fir.Initialiser.t Named.Alist.t)
    (statements : _ Fir.Statement.t list) : Ast.Compound_stm.t =
  decls ds @ Reify_stm.reify_compound statements

let func (f : _ Fir.Function.t Named.t) : Ast.External_decl.t =
  let id = Accessor.get Named.name f in
  let def = Accessor.get Named.value f in
  let parameters = Fir.Function.parameters def in
  let body_decls = Fir.Function.body_decls def in
  let body_stms = Fir.Function.body_stms def in
  `Fun
    { decl_specs= [`Void]
    ; signature= func_signature id parameters
    ; decls= []
    ; body= func_body body_decls body_stms }

let pp_func : type m. m Fir.Function.t Named.t Fmt.t =
 fun x -> Fmt.(using func Ast.External_decl.pp) x

let program (prog : _ Fir.Program.t) : Ast.Translation_unit.t =
  let globals = Fir.Program.globals prog in
  let functions = Named.list_of_alist (Fir.Program.functions prog) in
  List.concat [decls globals; List.map functions ~f:func]

let pp : type m. m Fir.Program.t Fmt.t =
 fun x -> Fmt.(using program Ast.Translation_unit.pp) x

let pp_litmus_raw : Act_litmus.Test.Raw.M(Fir.Litmus.Lang).t Fmt.t =
  Act_litmus.Pp.pp "C" Reify_prim.pp_constant pp_func

let pp_litmus : Fir.Litmus.Test.t Fmt.t =
  Fmt.using Fir.Litmus.Test.raw pp_litmus_raw
