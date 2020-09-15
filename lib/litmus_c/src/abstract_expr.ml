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
  module Fir = Act_fir
end

(** Type of recursive expression abstractors. *)
type mu = Ast.Expr.t -> Fir.Expression.t Or_error.t

let model_atomic_cmpxchg_expr (args : Ast.Expr.t list) ~(expr : mu) :
    Fir.Expression.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_cmpxchg ~expr
    >>| Fir.Expression.atomic_cmpxchg)

let model_atomic_fetch_expr (args : Ast.Expr.t list) ~(op : Fir.Op.Fetch.t)
    ~(expr : mu) : Fir.Expression.t Or_error.t =
  Or_error.(
    args
    |> Abstract_atomic.model_fetch ~expr ~op
    >>| Fir.Expression.atomic_fetch)

let model_atomic_load_expr (args : Ast.Expr.t list) ~(expr : mu) :
    Fir.Expression.t Or_error.t =
  ignore expr ;
  Or_error.(
    args |> Abstract_atomic.model_load >>| Fir.Expression.atomic_load)

let expr_call_table :
    (Ast.Expr.t list -> expr:mu -> Fir.Expression.t Or_error.t)
    Map.M(Ac.C_id).t
    Lazy.t =
  lazy
    (Map.of_alist_exn
       (module Ac.C_id)
       ( Abstract_atomic.fetch_call_alist model_atomic_fetch_expr
       @ [ ( Ac.C_id.of_string Abstract_atomic.cmpxchg_name
           , model_atomic_cmpxchg_expr )
         ; ( Ac.C_id.of_string Abstract_atomic.load_name
           , model_atomic_load_expr ) ] ))

let expr_call_handler (func_name : Ac.C_id.t) :
    (Ast.Expr.t list -> expr:mu -> Fir.Expression.t Or_error.t) Or_error.t =
  func_name
  |> Map.find (Lazy.force expr_call_table)
  |> Result.of_option
       ~error:
         (Error.create_s
            [%message
              "Unsupported function in expression position"
                ~got:(func_name : Ac.C_id.t)])

let function_call (func : Ast.Expr.t) (arguments : Ast.Expr.t list)
    ~(expr : mu) : Fir.Expression.t Or_error.t =
  Or_error.Let_syntax.(
    let%bind func_name = Abstract_prim.expr_to_identifier func in
    let%bind call_handler = expr_call_handler func_name in
    call_handler arguments ~expr)

let identifier_to_expr (id : Ac.C_id.t) : Fir.Expression.t =
  match Abstract_prim.identifier_to_constant id with
  | Some k ->
      Fir.Expression.constant k
  | None ->
      Fir.Expression.variable id

let bop : Operators.Bin.t -> Fir.Op.Binary.t Or_error.t =
  Or_error.(
    Fir.Op.Binary.(
      function
      | `Add ->
          return add
      | `Eq ->
          return eq
      | `Ne ->
          return ne
      | `Gt ->
          return gt
      | `Ge ->
          return ge
      | `Le ->
          return le
      | `Lt ->
          return lt
      | `Land ->
          return l_and
      | `Lor ->
          return l_or
      | `Sub ->
          return sub
      | `And ->
          return b_and
      | `Or ->
          return b_or
      | `Xor ->
          return b_xor
      | op ->
          error_s
            [%message
              "Unsupported binary operator" ~got:(op : Operators.Bin.t)]))

let prefix_op : Operators.Pre.t -> Fir.Op.Unary.t Or_error.t =
  Or_error.(
    function
    | `Lnot ->
        return Fir.Op.Unary.l_not
    | op ->
        error_s
          [%message
            "Unsupported prefix operator" ~got:(op : Operators.Pre.t)])

let model_binary (op : Operators.Bin.t) (l : Ast.Expr.t) (r : Ast.Expr.t)
    ~(expr : mu) : Fir.Expression.t Or_error.t =
  Or_error.map3 (bop op) (expr l) (expr r) ~f:Fir.Expression.bop

let model_prefix (op : Operators.Pre.t) (x : Ast.Expr.t) ~(expr : mu) :
    Fir.Expression.t Or_error.t =
  Or_error.map2 (prefix_op op) (expr x) ~f:Fir.Expression.uop

let rec model : Ast.Expr.t -> Fir.Expression.t Or_error.t = function
  | Brackets e ->
      model e
  | Binary (l, op, r) ->
      model_binary op l r ~expr:model
  | Constant k ->
      Or_error.(k |> Abstract_prim.constant >>| Fir.Expression.constant)
  | Identifier id ->
      Ok (identifier_to_expr id)
  | Prefix (`Deref, x) ->
      Or_error.(
        x |> Abstract_prim.expr_to_lvalue
        >>| Accessor.construct Fir.Lvalue.deref
        >>| Fir.Expression.lvalue)
  | Prefix (op, x) ->
      model_prefix op x ~expr:model
  | Call {func; arguments} ->
      function_call func arguments ~expr:model
  | ( Postfix _
    | Ternary _
    | Cast _
    | Subscript _
    | Field _
    | Sizeof_type _
    | String _ ) as e ->
      Or_error.error_s
        [%message "Unsupported expression" ~got:(e : Ast.Expr.t)]
