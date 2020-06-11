(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Ast = Act_litmus_c.Ast
  module Prim = Reify_prim
end

let known_call (name : string) (args : Ast.Expr.t list) : Ast.Expr.t =
  Call {func= Identifier (Act_common.C_id.of_string name); arguments= args}

let mem_order (mo : Mem_order.t) : Ast.Expr.t =
  Identifier (Act_common.C_id.of_string (Mem_order.to_string mo))

let cmpxchg (cmpxchg : 'e Atomic_cmpxchg.t) ~(expr : 'e -> Ast.Expr.t) :
    Ast.Expr.t =
  Atomic_cmpxchg.(
    known_call Convert_atomic.cmpxchg_name
      [ Prim.address (obj cmpxchg)
      ; Prim.address (expected cmpxchg)
      ; expr (desired cmpxchg)
      ; mem_order (succ cmpxchg)
      ; mem_order (fail cmpxchg) ])

let fence (fence : Atomic_fence.t) : Ast.Expr.t =
  let call = Convert_atomic.fence_name (Atomic_fence.mode fence) in
  known_call call [mem_order (Atomic_fence.mo fence)]

let fetch (f : 'e Atomic_fetch.t) ~(expr : 'e -> Ast.Expr.t) : Ast.Expr.t =
  Atomic_fetch.(
    known_call
      (Convert_atomic.fetch_name (op f))
      [Prim.address (obj f); expr (arg f); mem_order (mo f)])

let load (ld : Atomic_load.t) : Ast.Expr.t =
  Atomic_load.(
    known_call Convert_atomic.load_name
      [Prim.address (src ld); mem_order (mo ld)])

let store (st : Atomic_store.t) ~(expr : 'e -> Ast.Expr.t) : Ast.Expr.t =
  Atomic_store.(
    known_call Convert_atomic.store_name
      [Prim.address (dst st); expr (src st); mem_order (mo st)])

let xchg (xc : 'e Atomic_xchg.t) ~(expr : 'e -> Ast.Expr.t) : Ast.Expr.t =
  Atomic_xchg.(
    known_call Convert_atomic.xchg_name
      [Prim.address (obj xc); expr (desired xc); mem_order (mo xc)])

let reify_expr (x : 'e Atomic_expression.t) ~(expr : 'e -> Ast.Expr.t) :
    Ast.Expr.t =
  Atomic_expression.reduce x ~cmpxchg:(cmpxchg ~expr) ~fetch:(fetch ~expr)
    ~load ~xchg:(xchg ~expr)

let expr_stm (x : Ast.Expr.t) : Ast.Stm.t = Expr (Some x)

let reify_stm (x : Atomic_statement.t) ~(expr : Expression.t -> Ast.Expr.t) :
    Ast.Stm.t =
  x
  |> Atomic_statement.reduce ~cmpxchg:(cmpxchg ~expr) ~fetch:(fetch ~expr)
       ~fence ~store:(store ~expr) ~xchg:(xchg ~expr)
  |> expr_stm
