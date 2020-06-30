(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ast = Act_litmus_c.Ast

let atomic = Reify_atomic.reify_stm ~expr:Reify_expr.reify

let assign (asn : Assign.t) : Ast.Stm.t =
  let l = Assign.lvalue asn in
  let r = Assign.rvalue asn in
  Expr (Some Reify_expr.(Binary (Reify_prim.lvalue l, `Assign, reify r)))

let lift_stms : Ast.Stm.t list -> Ast.Compound_stm.t =
  List.map ~f:(fun s -> `Stm s)

let block_compound (type meta) (b : (meta, Ast.Stm.t) Block.t) :
    Ast.Compound_stm.t =
  lift_stms (Block.statements b)

let block (type meta) (b : (meta, Ast.Stm.t) Block.t) : Ast.Stm.t =
  Compound (block_compound b)

let ne_block (type meta) (b : (meta, Ast.Stm.t) Block.t) : Ast.Stm.t option =
  if Block.is_empty b then None else Some (block b)

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
  Prim_statement.value_map p ~assign ~atomic ~early_out ~procedure_call ~label
    ~goto ~nop

let if_stm (ifs : (_, Ast.Stm.t) If.t) : Ast.Stm.t =
  If
    { cond= Reify_expr.reify (If.cond ifs)
    ; t_branch= block (If.t_branch ifs)
    ; f_branch= ne_block (If.f_branch ifs) }

let while_loop (kind : Flow_block.While.t) (cond : Expression.t)
    (body : Ast.Compound_stm.t) : Ast.Stm.t =
  let cond' = Reify_expr.reify cond in
  let body' = Ast.Stm.Compound body in
  match kind with
  | While ->
      While (cond', body')
  | Do_while ->
      Do_while (body', cond')

let lock (kind : Flow_block.Lock.t) (body : Ast.Compound_stm.t) : Ast.Stm.t =
  match kind with Atomic -> Atomic body | Synchronized -> Synchronized body

let flow (fb : (_, Ast.Stm.t) Flow_block.t) : Ast.Stm.t =
  let body = block_compound (Flow_block.body fb) in
  match Flow_block.header fb with
  | Lock l ->
      lock l body
  | While (w, c) ->
      while_loop w c body

let reify (type meta) (m : meta Statement.t) : Ast.Stm.t =
  Statement.reduce m ~prim ~if_stm ~flow

(* Yay, value restriction... *)
let reify_compound (type meta) (m : meta Statement.t list) :
    Ast.Compound_stm.t =
  List.map ~f:(fun x -> `Stm (reify x)) m