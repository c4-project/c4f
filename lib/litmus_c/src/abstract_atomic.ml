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
  module Named = Ac.C_named
end

let cmpxchg_name : string = "atomic_compare_exchange_strong_explicit"

let fence_name (mode : Fir.Atomic_fence.Mode.t) : string =
  Printf.sprintf "atomic_%s_fence" (Fir.Atomic_fence.Mode.to_string mode)

let fetch_name (f : Fir.Op.Fetch.t) : string =
  Printf.sprintf "atomic_fetch_%s_explicit" (Fir.Op.Fetch.to_string f)

let load_name : string = "atomic_load_explicit"

let store_name : string = "atomic_store_explicit"

let xchg_name : string = "atomic_exchange_explicit"

let model_cmpxchg_with_args ~(raw_obj : Ast.Expr.t)
    ~(raw_expected : Ast.Expr.t) ~(raw_desired : Ast.Expr.t)
    ~(raw_succ : Ast.Expr.t) ~(raw_fail : Ast.Expr.t)
    ~(expr : Ast.Expr.t -> Fir.Expression.t Or_error.t) :
    Fir.Expression.t Fir.Atomic_cmpxchg.t Or_error.t =
  Or_error.Let_syntax.(
    let%map obj = Abstract_prim.expr_to_address raw_obj (* volatile A* *)
    and expected = Abstract_prim.expr_to_address raw_expected (* C* *)
    and desired = expr raw_desired (* C *)
    and succ = Abstract_prim.expr_to_memory_order raw_succ (* memory_order *)
    and fail =
      Abstract_prim.expr_to_memory_order raw_fail
      (* memory_order *)
    in
    Fir.Atomic_cmpxchg.make ~obj ~expected ~desired ~succ ~fail)

let model_cmpxchg (args : Ast.Expr.t list)
    ~(expr : Ast.Expr.t -> Fir.Expression.t Or_error.t) :
    Fir.Expression.t Fir.Atomic_cmpxchg.t Or_error.t =
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
    ~(raw_mo : Ast.Expr.t) ~(op : Fir.Op.Fetch.t)
    ~(expr : Ast.Expr.t -> Fir.Expression.t Or_error.t) :
    Fir.Expression.t Fir.Atomic_fetch.t Or_error.t =
  Or_error.Let_syntax.(
    let%map obj = Abstract_prim.expr_to_address raw_obj (* volatile A* *)
    and arg = expr raw_arg (* M *)
    and mo = Abstract_prim.expr_to_memory_order raw_mo (* memory_order *) in
    Fir.Atomic_fetch.make ~obj ~arg ~mo ~op)

let model_fetch (args : Ast.Expr.t list) ~(op : Fir.Op.Fetch.t)
    ~(expr : Ast.Expr.t -> Fir.Expression.t Or_error.t) :
    Fir.Expression.t Fir.Atomic_fetch.t Or_error.t =
  match args with
  | [raw_obj; raw_arg; raw_mo] ->
      model_fetch_with_args ~raw_obj ~raw_arg ~raw_mo ~expr ~op
  | _ ->
      Or_error.error_s
        [%message
          "Invalid arguments to atomic fetch" ~got:(args : Ast.Expr.t list)]

let model_fence_with_args ~(mode : Fir.Atomic_fence.Mode.t)
    ~(raw_mo : Ast.Expr.t) : Fir.Atomic_fence.t Or_error.t =
  Or_error.Let_syntax.(
    let%map mo = Abstract_prim.expr_to_memory_order raw_mo in
    Fir.Atomic_fence.make ~mode ~mo)

let model_fence (args : Ast.Expr.t list) ~(mode : Fir.Atomic_fence.Mode.t) :
    Fir.Atomic_fence.t Or_error.t =
  match args with
  | [raw_mo] ->
      model_fence_with_args ~mode ~raw_mo
  | args ->
      Or_error.error_s
        [%message
          "Invalid arguments to atomic fence" ~got:(args : Ast.Expr.t list)]

let model_load_with_args ~(raw_src : Ast.Expr.t) ~(raw_mo : Ast.Expr.t) :
    Fir.Atomic_load.t Or_error.t =
  Or_error.Let_syntax.(
    let%map src = Abstract_prim.expr_to_address raw_src
    and mo = Abstract_prim.expr_to_memory_order raw_mo in
    Fir.Atomic_load.make ~src ~mo)

let model_load : Ast.Expr.t list -> Fir.Atomic_load.t Or_error.t = function
  | [raw_src; raw_mo] ->
      model_load_with_args ~raw_src ~raw_mo
  | args ->
      Or_error.error_s
        [%message
          "Invalid arguments to atomic_load_explicit"
            ~got:(args : Ast.Expr.t list)]

let model_store_with_args ~(raw_dst : Ast.Expr.t) ~(raw_src : Ast.Expr.t)
    ~(raw_mo : Ast.Expr.t)
    ~(expr : Ast.Expr.t -> Fir.Expression.t Or_error.t) :
    Fir.Atomic_store.t Or_error.t =
  Or_error.Let_syntax.(
    let%map dst = Abstract_prim.expr_to_address raw_dst
    and src = expr raw_src
    and mo = Abstract_prim.expr_to_memory_order raw_mo in
    Fir.Atomic_store.make ~dst ~src ~mo)

let model_store (args : Ast.Expr.t list)
    ~(expr : Ast.Expr.t -> Fir.Expression.t Or_error.t) :
    Fir.Atomic_store.t Or_error.t =
  match args with
  | [raw_dst; raw_src; raw_mo] ->
      model_store_with_args ~raw_dst ~raw_src ~raw_mo ~expr
  | args ->
      Or_error.error_s
        [%message
          "Invalid arguments to atomic_store_explicit"
            ~got:(args : Ast.Expr.t list)]

let model_xchg_with_args ~(raw_obj : Ast.Expr.t) ~(raw_desired : Ast.Expr.t)
    ~(raw_mo : Ast.Expr.t)
    ~(expr : Ast.Expr.t -> Fir.Expression.t Or_error.t) :
    Fir.Expression.t Fir.Atomic_xchg.t Or_error.t =
  Or_error.Let_syntax.(
    let%map obj = Abstract_prim.expr_to_address raw_obj (* volatile A* *)
    and desired = expr raw_desired (* C *)
    and mo = Abstract_prim.expr_to_memory_order raw_mo (* memory_order *) in
    Fir.Atomic_xchg.make ~obj ~desired ~mo)

let model_xchg (args : Ast.Expr.t list)
    ~(expr : Ast.Expr.t -> Fir.Expression.t Or_error.t) :
    Fir.Expression.t Fir.Atomic_xchg.t Or_error.t =
  match args with
  | [raw_obj; raw_desired; raw_mo] ->
      model_xchg_with_args ~raw_obj ~raw_desired ~raw_mo ~expr
  | _ ->
      Or_error.error_s
        [%message
          "Invalid arguments to exchange" ~got:(args : Ast.Expr.t list)]
