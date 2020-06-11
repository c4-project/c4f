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

let cmpxchg_name : string = "atomic_compare_exchange_strong_explicit"

let fence_name (mode : Atomic_fence.Mode.t) : string =
  Printf.sprintf "atomic_%s_fence" (Atomic_fence.Mode.to_string mode)

let fetch_name (f : Op.Fetch.t) : string =
  Printf.sprintf "atomic_fetch_%s_explicit" (Op.Fetch.to_string f)

let load_name : string = "atomic_load_explicit"

let store_name : string = "atomic_store_explicit"

let xchg_name : string = "atomic_exchange_explicit"

let model_cmpxchg_with_args ~(raw_obj : Ast.Expr.t)
    ~(raw_expected : Ast.Expr.t) ~(raw_desired : Ast.Expr.t)
    ~(raw_succ : Ast.Expr.t) ~(raw_fail : Ast.Expr.t)
    ~(expr : Ast.Expr.t -> Expression.t Or_error.t) :
    Expression.t Atomic_cmpxchg.t Or_error.t =
  Or_error.Let_syntax.(
    let%map obj = Convert_prim.expr_to_address raw_obj (* volatile A* *)
    and expected = Convert_prim.expr_to_address raw_expected (* C* *)
    and desired = expr raw_desired (* C *)
    and succ = Convert_prim.expr_to_memory_order raw_succ (* memory_order *)
    and fail =
      Convert_prim.expr_to_memory_order raw_fail
      (* memory_order *)
    in
    Atomic_cmpxchg.make ~obj ~expected ~desired ~succ ~fail)

let model_cmpxchg (args : Ast.Expr.t list)
    ~(expr : Ast.Expr.t -> Expression.t Or_error.t) :
    Expression.t Atomic_cmpxchg.t Or_error.t =
  match args with
  | [raw_obj; raw_expected; raw_desired; raw_succ; raw_fail] ->
      model_cmpxchg_with_args ~raw_obj ~raw_expected ~raw_desired ~raw_succ
        ~raw_fail ~expr
  | _ ->
      Or_error.error_s
        [%message
          "Invalid arguments to atomic_compare_exchange_strong_explicit"
            ~got:(args : Ast.Expr.t list)]

let model_fetch_with_args ~(raw_obj : Ast.Expr.t) ~(raw_arg : Ast.Expr.t)
    ~(raw_mo : Ast.Expr.t) ~(op : Op.Fetch.t)
    ~(expr : Ast.Expr.t -> Expression.t Or_error.t) :
    Expression.t Atomic_fetch.t Or_error.t =
  Or_error.Let_syntax.(
    let%map obj = Convert_prim.expr_to_address raw_obj (* volatile A* *)
    and arg = expr raw_arg (* M *)
    and mo = Convert_prim.expr_to_memory_order raw_mo (* memory_order *) in
    Atomic_fetch.make ~obj ~arg ~mo ~op)

let model_fetch (args : Ast.Expr.t list) ~(op : Op.Fetch.t)
    ~(expr : Ast.Expr.t -> Expression.t Or_error.t) :
    Expression.t Atomic_fetch.t Or_error.t =
  match args with
  | [raw_obj; raw_arg; raw_mo] ->
      model_fetch_with_args ~raw_obj ~raw_arg ~raw_mo ~expr ~op
  | _ ->
      Or_error.error_s
        [%message
          "Invalid arguments to atomic fetch" ~got:(args : Ast.Expr.t list)]

let model_fence (args : Ast.Expr.t list) ~(mode : Atomic_fence.Mode.t) :
    Atomic_fence.t Or_error.t =
  match args with
  | [raw_mo] ->
      Or_error.Let_syntax.(
        let%map mo = Convert_prim.expr_to_memory_order raw_mo in
        Atomic_fence.make ~mode ~mo)
  | args ->
      Or_error.error_s
        [%message
          "Invalid arguments to atomic fence" ~got:(args : Ast.Expr.t list)]

let model_load : Ast.Expr.t list -> Atomic_load.t Or_error.t = function
  | [raw_src; raw_mo] ->
      Or_error.Let_syntax.(
        let%map src = Convert_prim.expr_to_address raw_src
        and mo = Convert_prim.expr_to_memory_order raw_mo in
        Atomic_load.make ~src ~mo)
  | args ->
      Or_error.error_s
        [%message
          "Invalid arguments to atomic_load_explicit"
            ~got:(args : Ast.Expr.t list)]

let model_store (args : Ast.Expr.t list)
    ~(expr : Ast.Expr.t -> Expression.t Or_error.t) :
    Atomic_store.t Or_error.t =
  match args with
  | [raw_dst; raw_src; raw_mo] ->
      Or_error.Let_syntax.(
        let%map dst = Convert_prim.expr_to_address raw_dst
        and src = expr raw_src
        and mo = Convert_prim.expr_to_memory_order raw_mo in
        Atomic_store.make ~dst ~src ~mo)
  | args ->
      Or_error.error_s
        [%message
          "Invalid arguments to atomic_store_explicit"
            ~got:(args : Ast.Expr.t list)]

let model_xchg_with_args ~(raw_obj : Ast.Expr.t) ~(raw_desired : Ast.Expr.t)
    ~(raw_mo : Ast.Expr.t) ~(expr : Ast.Expr.t -> Expression.t Or_error.t) :
    Expression.t Atomic_xchg.t Or_error.t =
  Or_error.Let_syntax.(
    let%map obj = Convert_prim.expr_to_address raw_obj (* volatile A* *)
    and desired = expr raw_desired (* C *)
    and mo = Convert_prim.expr_to_memory_order raw_mo (* memory_order *) in
    Atomic_xchg.make ~obj ~desired ~mo)

let model_xchg (args : Ast.Expr.t list)
    ~(expr : Ast.Expr.t -> Expression.t Or_error.t) :
    Expression.t Atomic_xchg.t Or_error.t =
  match args with
  | [raw_obj; raw_desired; raw_mo] ->
      model_xchg_with_args ~raw_obj ~raw_desired ~raw_mo ~expr
  | _ ->
      Or_error.error_s
        [%message
          "Invalid arguments to exchange" ~got:(args : Ast.Expr.t list)]
