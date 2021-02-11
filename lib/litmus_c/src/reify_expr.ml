(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module Fir = C4f_fir
end

let rel_bop : Fir.Op.Binary.Rel.t -> Operators.Bin.t = function
  | Eq -> `Eq
  | Ne -> `Ne
  | Gt -> `Gt
  | Ge -> `Ge
  | Le -> `Le
  | Lt -> `Lt

let arith_bop : Fir.Op.Binary.Arith.t -> Operators.Bin.t = function
  | Add -> `Add
  | Sub -> `Sub

let bitwise_bop : Fir.Op.Binary.Bitwise.t -> Operators.Bin.t = function
  | And -> `And
  | Or -> `Or
  | Xor -> `Xor

let logical_bop : Fir.Op.Binary.Logical.t -> Operators.Bin.t = function
  | And -> `Land
  | Or -> `Lor

let bop : Fir.Op.Binary.t -> Operators.Bin.t = function
  | Rel r -> rel_bop r
  | Arith a -> arith_bop a
  | Bitwise b -> bitwise_bop b
  | Logical l -> logical_bop l

let uop_pre : Fir.Op.Unary.t -> Operators.Pre.t = function L_not -> `Lnot

(** This module contains functions that try to calculate when brackets need
    to be inserted into expression ASTs.

    Working out where brackets need to go is a fairly complicated interaction
    of C operator precedence and associativity rules. *)
module Needs_brackets = struct
  let uop (x : Ast.Expr.t) ~(is_postfix : bool) : bool =
    match x with
    | Brackets _ ->
        (* Already has brackets. *)
        false
    | Identifier _ | Constant _ | String _ ->
        (* These are all primitives. *)
        false
    | Call _ | Subscript _ | Field _ | Postfix _ ->
        (* These always bind tighter than, or at the same level as, the
           operator; as a result, they need no brackets. *)
        false
    | Prefix _ | Sizeof_type _ | Cast _ ->
        (* If we're prefix, then these bind equally tightly, and we don't
           need brackets. If we're postfix, they bind looser than us, so we
           need brackets. *)
        is_postfix
    | Binary _ | Ternary _ ->
        (* These bind looser than prefixes and postfixes, so need brackets. *)
        true

  let uop_pre : Ast.Expr.t -> bool = uop ~is_postfix:false

  let uop_post : Ast.Expr.t -> bool = uop ~is_postfix:true

  (* NB: This works ATM because all of the bops are left-associative and have
     the same precedence, and will need refining if any right-associative
     Bops (assignments!) appear. *)
  let bop (o : Operators.Bin.t) (operand : Ast.Expr.t) ~(is_left : bool) :
      bool =
    match operand with
    | Brackets _ ->
        (* Already has brackets. *)
        false
    | Identifier _ | Constant _ | String _ ->
        (* These are all primitives. *)
        false
    | Prefix _ | Postfix _ ->
        (* All postfix operators bind tighter than binaries. *)
        false
    | Cast _ | Call _ | Subscript _ | Field _ | Sizeof_type _ ->
        (* All of these also bind tighter than binaries. *)
        false
    | Ternary _ ->
        (* These bind looser than binaries. *)
        true
    | Binary (_, #Operators.Assign.t, _) ->
        (* At time of writing, assignments shouldn't turn up in the middle of
           expressions. However, in case they do, we'll be conservative and
           add brackets (they tend to bind looser than other binary
           expressions, anyway). *)
        true
    | Binary (_, o', _) ->
        (* We add brackets if this expression binds looser than its parent,
           or, as [o] and [o'] at this stage should _both_ be
           left-associative, if the inner binary is appearing on the LHS of
           the outer binary. *)
        Operators.Bin.(
          binds_tighter o ~than:o' || (binds_same o o' && not is_left))

  let ternary_if (x : Ast.Expr.t) : bool =
    (* The only thing that binds looser than a ternary is another ternary. *)
    match x with
    | Ternary _ -> true
    | Brackets _ | Identifier _ | Constant _ | String _ | Call _
     |Subscript _ | Field _ | Postfix _ | Binary _ | Prefix _
     |Sizeof_type _ | Cast _ ->
        false

  let maybe_bracket (expr : Ast.Expr.t) ~(f : Ast.Expr.t -> bool) :
      Ast.Expr.t =
    if f expr then Ast.Expr.Brackets expr else expr
end

let bop (op : Fir.Op.Binary.t) (l : Ast.Expr.t) (r : Ast.Expr.t) : Ast.Expr.t
    =
  let op' = bop op in
  let l' = Needs_brackets.(maybe_bracket ~f:(bop op' ~is_left:true)) l in
  let r' = Needs_brackets.(maybe_bracket ~f:(bop op' ~is_left:false)) r in
  Ast.Expr.Binary (l', op', r')

let uop (op : Fir.Op.Unary.t) (x : Ast.Expr.t) : Ast.Expr.t =
  (* We don't have any postfix operators in FIR yet. *)
  let op' = uop_pre op in
  let x' = Needs_brackets.(maybe_bracket ~f:uop_pre) x in
  Ast.Expr.Prefix (op', x')

let ternary ({if_; then_; else_} : Ast.Expr.t Fir.Expr_ternary.t) :
    Ast.Expr.t =
  let cond = Needs_brackets.(maybe_bracket ~f:ternary_if if_) in
  (* TODO(@MattWindsor91): fix this redundancy somehow *)
  Ast.Expr.Ternary {cond; t_expr= then_; f_expr= else_}

let reify (x : Fir.Expression.t) : Ast.Expr.t =
  let atomic = Reify_atomic.reify_expr ~expr:Fn.id in
  Reify_prim.(
    Fir.Expression.reduce x ~constant ~address ~atomic ~bop ~uop ~ternary)

let pp : Fir.Expression.t Fmt.t = Fmt.using reify Ast.Expr.pp

let postfix (op : Operators.Post.t) (x : Ast.Expr.t) : Ast.Expr.t =
  Postfix (Needs_brackets.(maybe_bracket ~f:uop_post x), op)
