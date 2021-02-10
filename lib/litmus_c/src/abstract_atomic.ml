(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let cmpxchg_name (strength : Fir.Atomic_cmpxchg.Strength.t) : string =
  Printf.sprintf "atomic_compare_exchange_%s_explicit"
    (Fir.Atomic_cmpxchg.Strength.to_string strength)

let fence_name (mode : Fir.Atomic_fence.Mode.t) : string =
  Printf.sprintf "atomic_%s_fence" (Fir.Atomic_fence.Mode.to_string mode)

let fetch_name : Fir.Op.Fetch.t -> string = function
  | `Xchg ->
      "atomic_exchange_explicit"
  | f ->
      Printf.sprintf "atomic_fetch_%s_explicit" (Fir.Op.Fetch.to_string f)

let load_name : string = "atomic_load_explicit"

let store_name : string = "atomic_store_explicit"

let cmpxchg_call_alist
    (modeller :
      Ast.Expr.t list -> strength:Fir.Atomic_cmpxchg.Strength.t -> 'a) :
    (Common.C_id.t, Ast.Expr.t list -> 'a) List.Assoc.t =
  List.map (Fir.Atomic_cmpxchg.Strength.all_list ()) ~f:(fun strength ->
      let name = Common.C_id.of_string (cmpxchg_name strength) in
      (name, modeller ~strength))

let fence_call_alist
    (modeller : Ast.Expr.t list -> mode:Fir.Atomic_fence.Mode.t -> 'a) :
    (Common.C_id.t, Ast.Expr.t list -> 'a) List.Assoc.t =
  List.map (Fir.Atomic_fence.Mode.all_list ()) ~f:(fun mode ->
      let name = Common.C_id.of_string (fence_name mode) in
      (name, modeller ~mode))

let fetch_call_alist (modeller : Ast.Expr.t list -> op:Fir.Op.Fetch.t -> 'a)
    : (Common.C_id.t, Ast.Expr.t list -> 'a) List.Assoc.t =
  List.map (Fir.Op.Fetch.all_list ()) ~f:(fun op ->
      let name = Common.C_id.of_string (fetch_name op) in
      (name, modeller ~op))

let model_cmpxchg_with_args ~(strength : Fir.Atomic_cmpxchg.Strength.t)
    ~(raw_obj : Ast.Expr.t) ~(raw_expected : Ast.Expr.t)
    ~(raw_desired : Ast.Expr.t) ~(raw_succ : Ast.Expr.t)
    ~(raw_fail : Ast.Expr.t)
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
    {Fir.Atomic_cmpxchg.obj; expected; desired; strength; succ; fail})

let model_cmpxchg (args : Ast.Expr.t list)
    ~(strength : Fir.Atomic_cmpxchg.Strength.t)
    ~(expr : Ast.Expr.t -> Fir.Expression.t Or_error.t) :
    Fir.Expression.t Fir.Atomic_cmpxchg.t Or_error.t =
  match args with
  | [raw_obj; raw_expected; raw_desired; raw_succ; raw_fail] ->
      model_cmpxchg_with_args ~strength ~raw_obj ~raw_expected ~raw_desired
        ~raw_succ ~raw_fail ~expr
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
