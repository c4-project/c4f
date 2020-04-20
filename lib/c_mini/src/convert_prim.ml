(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ac = Act_common
  module Tx = Travesty_base_exts
  module Ast = Act_c_lang.Ast
  module Named = Ac.C_named
end

let constant : Act_c_lang.Ast_basic.Constant.t -> Constant.t Or_error.t =
  function
  | Integer k ->
      Or_error.return (Constant.int k)
  | Char _ | Float _ ->
      Or_error.error_string "Unsupported constant type"

let defined_types : (Ac.C_id.t, Type.Basic.t) List.Assoc.t Lazy.t =
  lazy
    Type.Basic.
      [ (Ac.C_id.of_string "atomic_bool", bool ~atomic:true ())
      ; (Ac.C_id.of_string "atomic_int", int ~atomic:true ())
      ; (Ac.C_id.of_string "bool", bool ()) ]

let qualifiers_to_basic_type (quals : [> Ast.Decl_spec.t] list) :
    Type.Basic.t Or_error.t =
  let open Or_error.Let_syntax in
  match%bind Tx.List.one quals with
  | `Int ->
      return (Type.Basic.int ())
  | `Defined_type t ->
      t
      |> List.Assoc.find ~equal:Ac.C_id.equal (Lazy.force defined_types)
      |> Result.of_option
           ~error:
             (Error.create_s
                [%message "Unknown defined type" ~got:(t : Ac.C_id.t)])
  | #Ast.Type_spec.t as spec ->
      Or_error.error_s
        [%message
          "This type isn't supported (yet)" ~got:(spec : Ast.Type_spec.t)]
  | #Act_c_lang.Ast_basic.Type_qual.t as qual ->
      Or_error.error_s
        [%message
          "This type qualifier isn't supported (yet)"
            ~got:(qual : Act_c_lang.Ast_basic.Type_qual.t)]
  | #Act_c_lang.Ast_basic.Storage_class_spec.t as spec ->
      Or_error.error_s
        [%message
          "This storage-class specifier isn't supported (yet)"
            ~got:(spec : Act_c_lang.Ast_basic.Storage_class_spec.t)]

let declarator_to_id :
    Ast.Declarator.t -> (Act_common.C_id.t * bool) Or_error.t = function
  | {pointer= Some [[]]; direct= Id id} ->
      Or_error.return (id, true)
  | {pointer= Some _; _} as decl ->
      Or_error.error_s
        [%message
          "Complex pointers not supported yet"
            ~declarator:(decl : Ast.Declarator.t)]
  | {pointer= None; direct= Id id} ->
      Or_error.return (id, false)
  | x ->
      Or_error.error_s
        [%message
          "Unsupported direct declarator"
            ~got:(x.direct : Ast.Direct_declarator.t)]

let value_of_initialiser : Ast.Initialiser.t -> Constant.t Or_error.t =
  function
  | Assign (Constant v) ->
      (* TODO(@MattWindsor91): Boolean initialisers aren't covered by this
         case, as C99 Boolean 'constant's are identifiers. *)
      constant v
  | Assign x ->
      Or_error.error_s
        [%message
          "Expression not supported (must be constant)" (x : Ast.Expr.t)]
  | List _ ->
      Or_error.error_string "List initialisers not supported"

let decl (d : Act_c_lang.Ast.Decl.t) : Initialiser.t Named.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind basic_type = qualifiers_to_basic_type d.qualifiers in
    let%bind idecl = Tx.List.one d.declarator in
    let%bind name, pointer = declarator_to_id idecl.declarator in
    let%map value =
      Tx.Option.With_errors.map_m idecl.initialiser ~f:value_of_initialiser
    in
    let ty = Type.of_basic ~pointer basic_type in
    Named.make ~name (Initialiser.make ~ty ?value ()))

let param_decl : Ast.Param_decl.t -> Type.t Named.t Or_error.t = function
  | {declarator= `Abstract _; _} ->
      Or_error.error_string "Abstract parameter declarators not supported"
  | {qualifiers; declarator= `Concrete declarator} ->
      let open Or_error.Let_syntax in
      let%map basic_type = qualifiers_to_basic_type qualifiers
      and name, pointer = declarator_to_id declarator in
      let value = Type.of_basic ~pointer basic_type in
      Named.make value ~name

let rec expr_to_lvalue : Ast.Expr.t -> Lvalue.t Or_error.t = function
  | Identifier id ->
      Or_error.return (Lvalue.variable id)
  | Brackets expr ->
      expr_to_lvalue expr
  | Prefix (`Deref, expr) ->
      Or_error.(expr |> expr_to_lvalue >>| Lvalue.deref)
  | ( Prefix _
    | Postfix _
    | Binary _
    | Ternary _
    | Cast _
    | Call _
    | Subscript _
    | Field _
    | Sizeof_type _
    | String _
    | Constant _ ) as e ->
      Or_error.error_s
        [%message "Expected an lvalue here" ~got:(e : Ast.Expr.t)]

let rec expr_to_address : Ast.Expr.t -> Address.t Or_error.t = function
  | Prefix (`Ref, expr) ->
      Or_error.(expr |> expr_to_address >>| Address.ref)
  | expr ->
      Or_error.(expr |> expr_to_lvalue >>| Address.lvalue)

let lvalue_to_identifier (lv : Lvalue.t) : Act_common.C_id.t Or_error.t =
  if Lvalue.is_deref lv then
    Or_error.error_s [%message "Expected identifier" ~got:(lv : Lvalue.t)]
  else Or_error.return (Lvalue.variable_of lv)

let expr_to_identifier (expr : Ast.Expr.t) : Act_common.C_id.t Or_error.t =
  Or_error.(expr |> expr_to_lvalue >>= lvalue_to_identifier)

let expr_to_memory_order (expr : Ast.Expr.t) : Mem_order.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind id = expr_to_identifier expr in
    id |> Ac.C_id.to_string |> Mem_order.of_string_option
    |> Result.of_option
      ~error:
        (Error.create_s
           [%message
             "Unsupported memory order" ~got:(id : Act_common.C_id.t)])
  )
