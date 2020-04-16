(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ast = Act_c_lang.Ast

let known_call_stm (name : string) (args : Ast.Expr.t list) : Ast.Stm.t =
  Expr (Some (Reify_expr.known_call name args))

let cmpxchg (cmpxchg : Atomic_cmpxchg.t) : Ast.Stm.t =
  known_call_stm "atomic_compare_exchange_strong_explicit"
    Reify_expr.
      [ address (Atomic_cmpxchg.obj cmpxchg)
      ; address (Atomic_cmpxchg.expected cmpxchg)
      ; reify (Atomic_cmpxchg.desired cmpxchg)
      ; mem_order (Atomic_cmpxchg.succ cmpxchg)
      ; mem_order (Atomic_cmpxchg.fail cmpxchg) ]

let function_of_fence_mode : Atomic_fence.Mode.t -> string = function
  | Signal ->
      "atomic_signal_fence"
  | Thread ->
      "atomic_thread_fence"

let fence (fence : Atomic_fence.t) : Ast.Stm.t =
  let call = function_of_fence_mode (Atomic_fence.mode fence) in
  known_call_stm call Reify_expr.[mem_order (Atomic_fence.mo fence)]

let store (st : Atomic_store.t) : Ast.Stm.t =
  known_call_stm "atomic_store_explicit"
    Reify_expr.
      [ address (Atomic_store.dst st)
      ; reify (Atomic_store.src st)
      ; mem_order (Atomic_store.mo st) ]

let atomic : Atomic_statement.t -> Ast.Stm.t =
  Atomic_statement.reduce ~cmpxchg ~fence ~store

let assign (asn : Assign.t) : Ast.Stm.t =
  let l = Assign.lvalue asn in
  let r = Assign.rvalue asn in
  Expr (Some Reify_expr.(Binary (lvalue l, `Assign, reify r)))

let lift_stms (type stm) (stm : stm -> Ast.Stm.t) (xs : stm list) :
    Ast.Compound_stm.t =
  List.map ~f:(fun s -> `Stm (stm s)) xs

let block (type meta stm) (stm : stm -> Ast.Stm.t) (b : (meta, stm) Block.t)
    : Ast.Stm.t =
  Compound (lift_stms stm (Block.statements b))

let ne_block (type meta stm) (stm : stm -> Ast.Stm.t)
    (b : (meta, stm) Block.t) : Ast.Stm.t option =
  if Block.is_empty b then None else Some (block stm b)

let nop (_ : 'meta) : Ast.Stm.t = Ast.Stm.Expr None

let early_out : Early_out.t -> Ast.Stm.t = function
  | Break ->
      Ast.Stm.Break
  | Continue ->
      Ast.Stm.Continue
  | Return ->
      Ast.Stm.Return None

let label (l : Act_common.C_id.t) : Ast.Stm.t =
  (* This might need revisiting later. *)
  Label (Normal l, Expr None)

let goto (l : Act_common.C_id.t) : Ast.Stm.t = Goto l

let procedure_call (c : Call.t) : Ast.Stm.t =
  Ast.Stm.Expr
    (Some
       (Ast.Expr.Call
          { func= Identifier (Call.function_id c)
          ; arguments= List.map ~f:Reify_expr.reify (Call.arguments c) }))

let prim ((_, p) : _ * Prim_statement.t) : Ast.Stm.t =
  Prim_statement.reduce p ~assign ~atomic ~early_out ~procedure_call ~label
    ~goto ~nop

let rec reify : _ Statement.t -> Ast.Stm.t =
  Statement.reduce ~prim ~if_stm ~while_loop

and if_stm (ifs : _ Statement.If.t) : Ast.Stm.t =
  If
    { cond= Reify_expr.reify (Statement.If.cond ifs)
    ; t_branch= block reify (Statement.If.t_branch ifs)
    ; f_branch= ne_block reify (Statement.If.f_branch ifs) }

and while_loop (loop : _ Statement.While.t) : Ast.Stm.t =
  let cond = Reify_expr.reify (Statement.While.cond loop)
  and body = block reify (Statement.While.body loop) in
  match Statement.While.kind loop with
  | `While ->
      While (cond, body)
  | `Do_while ->
      Do_while (body, cond)

(* Yay, value restriction... *)
let reify_compound (type meta) (m : meta Statement.t list) :
    Ast.Compound_stm.t =
  lift_stms reify m
