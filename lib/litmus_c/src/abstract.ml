(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel (* for Fqueue *)

open struct
  module Ac = Act_common
  module Fir = Act_fir
  module Tx = Travesty_base_exts
  module Named = Ac.C_named
end

(** [ensure_functions xs] makes sure that each member of [xs] is a function
    definition. *)
let ensure_functions :
    Ast.External_decl.t list -> Ast.Function_def.t list Or_error.t =
  Tx.Or_error.combine_map ~f:(function
    | `Fun f ->
        Ok f
    | d ->
        Or_error.error_s
          [%message "Expected a function" ~got:(d : Ast.External_decl.t)])

let validate_func_void_type (f : Ast.Function_def.t) : Validate.t =
  match f.decl_specs with
  | [`Void] ->
      Validate.pass
  | xs ->
      Validate.fail_s
        [%message "Expected 'void'" ~got:(xs : Ast.Decl_spec.t list)]

let validate_func_no_knr : Ast.Function_def.t Validate.check =
  Validate.booltest
    (fun f -> List.is_empty f.Ast.Function_def.decls)
    ~if_false:"K&R style function definitions not supported"

let validate_func : Ast.Function_def.t Validate.check =
  Validate.all [validate_func_void_type; validate_func_no_knr]

let param_type_list :
    Ast.Param_type_list.t -> Fir.Type.t Named.Alist.t Or_error.t = function
  | {style= `Variadic; _} ->
      Or_error.error_string "Variadic arguments not supported"
  | {style= `Normal; params} ->
      Or_error.(
        params
        |> Tx.Or_error.combine_map ~f:Abstract_prim.param_decl
        >>| Named.alist_of_list)

let func_signature :
       Ast.Declarator.t
    -> (Act_common.C_id.t * Fir.Type.t Named.Alist.t) Or_error.t = function
  | {pointer= Some _; _} ->
      Or_error.error_string "Pointers not supported yet"
  | {pointer= None; direct= Fun_decl (Id name, param_list)} ->
      Or_error.(param_list |> param_type_list >>| Tuple2.create name)
  | {pointer= None; direct= Fun_call (Id name, param_list)} as x -> (
    match param_list with
    | [] ->
        Ok (name, [])
    | _ ->
        Or_error.error_s
          [%message
            "K&R style function parameter lists not supported"
              ~got:(x.direct : Ast.Direct_declarator.t)] )
  | x ->
      Or_error.error_s
        [%message
          "Unsupported function declarator"
            ~got:(x.direct : Ast.Direct_declarator.t)]

let func_body (body : Ast.Compound_stm.t) :
    (Fir.Initialiser.t Named.Alist.t * unit Fir.Statement.t list) Or_error.t
    =
  Or_error.Let_syntax.(
    let%bind ast_decls, ast_nondecls = Abstract_prim.sift_decls body in
    let%bind ast_stms = Abstract_stm.ensure_statements ast_nondecls in
    let%map decls = Tx.Or_error.combine_map ~f:Abstract_prim.decl ast_decls
    and stms = Tx.Or_error.combine_map ~f:Abstract_stm.model ast_stms in
    (Named.alist_of_list decls, stms))

let func (f : Ast.Function_def.t) : unit Fir.Function.t Named.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind () = Validate.result (validate_func f) in
    let%map name, parameters = func_signature f.signature
    and body_decls, body_stms = func_body f.body in
    Named.make ~name
      (Fir.Function.make ~parameters ~body_decls ~body_stms ()))

let translation_unit (prog : Ast.Translation_unit.t) :
    unit Fir.Program.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind ast_decls, ast_nondecls = Abstract_prim.sift_decls prog in
    let%bind ast_funs = ensure_functions ast_nondecls in
    let%map global_list =
      Tx.Or_error.combine_map ~f:Abstract_prim.decl ast_decls
    and function_list = Tx.Or_error.combine_map ~f:func ast_funs in
    let globals = Named.alist_of_list global_list in
    let functions = Named.alist_of_list function_list in
    Fir.Program.make ~globals ~functions)

module Litmus_conv = Act_litmus.Convert.Make (struct
  module From = struct
    include Ast.Litmus
    module Lang = Ast.Litmus_lang
  end

  module To = Fir.Litmus.Test

  let program : From.Lang.Program.t -> To.Lang.Program.t Or_error.t = func

  let constant : From.Lang.Constant.t -> To.Lang.Constant.t Or_error.t =
    Abstract_prim.constant
end)

let litmus_post :
       Ast_basic.Constant.t Act_litmus.Postcondition.t
    -> Fir.Constant.t Act_litmus.Postcondition.t Or_error.t =
  Litmus_conv.convert_post

let litmus : Ast.Litmus.t -> Fir.Litmus.Test.t Or_error.t =
  Litmus_conv.convert

let litmus_of_raw_ast (ast : Act_litmus.Ast.M(Ast.Litmus_lang).t) =
  Or_error.(ast |> Ast.Litmus.of_ast >>= litmus)
