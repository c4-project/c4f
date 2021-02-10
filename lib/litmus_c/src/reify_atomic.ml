(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

open struct
  module Prim = Reify_prim
end

let known_call (name : string) (args : Ast.Expr.t list) : Ast.Expr.t =
  Call {func= Identifier (Common.C_id.of_string name); arguments= args}

let mem_order (mo : Fir.Mem_order.t) : Ast.Expr.t =
  Identifier (Common.C_id.of_string (Fir.Mem_order.to_string mo))

let cmpxchg (cmpxchg : 'e Fir.Atomic_cmpxchg.t) ~(expr : 'e -> Ast.Expr.t) :
    Ast.Expr.t =
  Fir.(
    Atomic_cmpxchg.(
      known_call
        (Abstract_atomic.cmpxchg_name cmpxchg.strength)
        [ Prim.address cmpxchg.obj
        ; Prim.address cmpxchg.expected
        ; expr cmpxchg.desired
        ; mem_order cmpxchg.succ
        ; mem_order cmpxchg.fail ]))

let fence (fence : Fir.Atomic_fence.t) : Ast.Expr.t =
  let call = Abstract_atomic.fence_name (Fir.Atomic_fence.mode fence) in
  known_call call [mem_order (Fir.Atomic_fence.mo fence)]

let fetch (f : 'e Fir.Atomic_fetch.t) ~(expr : 'e -> Ast.Expr.t) : Ast.Expr.t
    =
  Fir.(
    Atomic_fetch.(
      known_call
        (Abstract_atomic.fetch_name f.op)
        [Prim.address f.obj; expr f.arg; mem_order f.mo]))

let load (ld : Fir.Atomic_load.t) : Ast.Expr.t =
  Fir.(
    Atomic_load.(
      known_call Abstract_atomic.load_name
        [Prim.address ld.src; mem_order ld.mo]))

let store (st : Fir.Atomic_store.t) ~(expr : 'e -> Ast.Expr.t) : Ast.Expr.t =
  Fir.(
    Atomic_store.(
      known_call Abstract_atomic.store_name
        [Prim.address st.dst; expr st.src; mem_order st.mo]))

let reify_expr (x : 'e Fir.Atomic_expression.t) ~(expr : 'e -> Ast.Expr.t) :
    Ast.Expr.t =
  Fir.Atomic_expression.reduce x ~cmpxchg:(cmpxchg ~expr)
    ~fetch:(fetch ~expr) ~load

let expr_stm (x : Ast.Expr.t) : Ast.Stm.t = Expr (Some x)

let reify_stm (x : Fir.Atomic_statement.t)
    ~(expr : Fir.Expression.t -> Ast.Expr.t) : Ast.Stm.t =
  x
  |> Fir.Atomic_statement.value_map ~cmpxchg:(cmpxchg ~expr)
       ~fetch:(fetch ~expr) ~fence ~store:(store ~expr)
  |> expr_stm
