(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

(* A lot of this module involves dealing with the prospect of one FIR
   statement expanding into multiple C statements; hence why reified blocks
   have statements that themselves are lists of statements. At time of
   writing (note that comments lie, including this one), the types of FIR
   statement that introduce multiple C statements are:

   - implicit flow blocks (these expand to the statements they contain) *)

let atomic = Reify_atomic.reify_stm ~expr:Reify_expr.reify

let assign_expr (asn : Fir.Assign.t) : Ast.Expr.t =
  let dst = Reify_prim.lvalue asn.@(Fir.Assign.dst) in
  match asn.@(Fir.Assign.src) with
  | Inc ->
      Reify_expr.postfix `Inc dst
  | Dec ->
      Reify_expr.postfix `Dec dst
  | Expr e ->
      Binary (dst, `Assign, Reify_expr.reify e)

let assign (asn : Fir.Assign.t) : Ast.Stm.t = Expr (Some (assign_expr asn))

let lift_stms : Ast.Stm.t list -> Ast.Compound_stm.t =
  List.map ~f:(fun s -> `Stm s)

let flat_statements =
  [%accessor Fir.Block.each_statement @> Accessor.List.each]

let merge_stms (b : ('meta, Ast.Stm.t list) Fir.Block.t) : Ast.Stm.t list =
  b.@*(flat_statements)

let block_compound (type meta) (b : (meta, Ast.Stm.t list) Fir.Block.t) :
    Ast.Compound_stm.t =
  lift_stms (merge_stms b)

let block (type meta) (b : (meta, Ast.Stm.t list) Fir.Block.t) : Ast.Stm.t =
  Compound (block_compound b)

let ne_block (type meta) (b : (meta, Ast.Stm.t list) Fir.Block.t) :
    Ast.Stm.t option =
  (* We can't use Fir.Block.is_empty here, as it'd suggest a block whose
     statement list is [[]] is not empty. *)
  if Accessor.is_empty flat_statements b then None else Some (block b)

let nop (_ : 'meta) : Ast.Stm.t = Ast.Stm.Expr None

let early_out : Fir.Early_out.t -> Ast.Stm.t = function
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

let procedure_call (c : Fir.Call.t) : Ast.Stm.t =
  Ast.Stm.Expr
    (Some
       (Ast.Expr.Call
          { func= Identifier (Fir.Call.function_id c)
          ; arguments= List.map ~f:Reify_expr.reify (Fir.Call.arguments c) }))

let prim ((_, p) : _ * Fir.Prim_statement.t) : Ast.Stm.t list =
  [ Fir.Prim_statement.value_map p ~assign ~atomic ~early_out ~procedure_call
      ~label ~goto ~nop ]

let if_stm (ifs : (_, Ast.Stm.t list) Fir.If.t) : Ast.Stm.t list =
  [ If
      { cond= Reify_expr.reify (Fir.If.cond ifs)
      ; t_branch= block (Fir.If.t_branch ifs)
      ; f_branch= ne_block (Fir.If.f_branch ifs) } ]

let for_loop (header : Fir.Flow_block.For.t) (body : Ast.Compound_stm.t) :
    Ast.Stm.t =
  let init = Option.map ~f:assign_expr header.init in
  let cond = Option.map ~f:Reify_expr.reify header.cmp in
  let update = Option.map ~f:assign_expr header.update in
  let body = Ast.Stm.Compound body in
  For {init; cond; update; body}

let while_loop (kind : Fir.Flow_block.While.t) (cond : Fir.Expression.t)
    (body : Ast.Compound_stm.t) : Ast.Stm.t =
  let cond' = Reify_expr.reify cond in
  let body' = Ast.Stm.Compound body in
  match kind with
  | While ->
      While (cond', body')
  | Do_while ->
      Do_while (body', cond')

let lock (kind : Fir.Flow_block.Lock.t) (body : Ast.Compound_stm.t) :
    Ast.Stm.t =
  match kind with Atomic -> Atomic body | Synchronized -> Synchronized body

let flow (fb : (_, Ast.Stm.t list) Fir.Flow_block.t) : Ast.Stm.t list =
  let body' = Fir.Flow_block.body fb in
  let body = block_compound body' in
  match Fir.Flow_block.header fb with
  | For f ->
      [for_loop f body]
  | Lock l ->
      [lock l body]
  | While (w, c) ->
      [while_loop w c body]
  | Explicit ->
      [Compound body]
  | Implicit ->
      merge_stms body'

let reify (type meta) (m : meta Fir.Statement.t) : Ast.Stm.t list =
  Fir.Statement.reduce m ~prim ~if_stm ~flow

let pp : type meta. meta Fir.Statement.t Fmt.t =
 fun x -> Fmt.(using reify (list ~sep:sp Ast.Stm.pp)) x

(* Yay, value restriction... *)
let reify_compound (type meta) (m : meta Fir.Statement.t list) :
    Ast.Compound_stm.t =
  List.concat_map ~f:(fun x -> lift_stms (reify x)) m
