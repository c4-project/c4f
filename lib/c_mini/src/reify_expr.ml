(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ast = Act_c_lang.Ast

let bop : Op.Binary.t -> Act_c_lang.Operators.Bin.t = function
  | Eq ->
      `Eq
  | Arith Add ->
      `Add
  | Arith Sub ->
      `Sub
  | Bitwise And ->
      `And
  | Bitwise Or ->
      `Or
  | Bitwise Xor ->
      `Xor
  | Logical And ->
      `Land
  | Logical Or ->
      `Lor

let uop_pre : Op.Unary.t -> Act_c_lang.Operators.Pre.t = function
  | L_not ->
      `Lnot

(** This module contains functions that try to calculate when brackets need
    to be inserted into expression ASTs.

    Working out where brackets need to go is a fairly complicated interaction
    of C operator precedence and associativity rules. *)
module Needs_brackets = struct
  let uop_pre : Ast.Expr.t -> bool = function
    | Brackets _ ->
        (* Already has brackets. *)
        false
    | Identifier _ | Constant _ | String _ ->
        (* These are all primitives. *)
        false
    | Postfix _ ->
        (* All postfix operators bind tighter than prefixes. *)
        false
    | Binary _ ->
        (* All binary operators bind looser than prefixes, and so need
           brackets. *)
        true
    | Ternary _ ->
        (* These bind looser than prefixes. *)
        true
    | Prefix _ ->
        (* All prefixes bind equally tightly, and I'm not convinced there are
           any cases in which brackets are needed, even when the . *)
        false
    | Cast _ | Call _ | Subscript _ | Field _ | Sizeof_type _ ->
        (* These bind equally tightly to prefixes, but at time of writing I
           was a little confused as to if and when brackets were needed, so
           this is a conservative overapproximation. *)
        true

  (* NB: This works ATM because all of the bops are left-associative and have
     the same precedence, and will need refining if any right-associative
     Bops (assignments!) appear. *)
  let bop (o : Act_c_lang.Operators.Bin.t) (operand : Ast.Expr.t)
      ~(is_left : bool) : bool =
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
    | Binary (_, #Act_c_lang.Operators.Assign.t, _) ->
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
        Act_c_lang.Operators.Bin.(
          binds_tighter o ~than:o' || (binds_same o o' && not is_left))

  let maybe_bracket (expr : Ast.Expr.t) ~(f : Ast.Expr.t -> bool) :
      Ast.Expr.t =
    if f expr then Ast.Expr.Brackets expr else expr
end

let bop (op : Op.Binary.t) (l : Ast.Expr.t) (r : Ast.Expr.t) : Ast.Expr.t =
  let op' = bop op in
  let l' = Needs_brackets.(maybe_bracket ~f:(bop op' ~is_left:true)) l in
  let r' = Needs_brackets.(maybe_bracket ~f:(bop op' ~is_left:false)) r in
  Ast.Expr.Binary (l', op', r')

let uop (op : Op.Unary.t) (x : Ast.Expr.t) : Ast.Expr.t =
  (* We don't have any postfix operators in mini-C yet. *)
  let op' = uop_pre op in
  let x' = Needs_brackets.(maybe_bracket ~f:uop_pre) x in
  Ast.Expr.Prefix (op', x')

let reify (x : Expression.t) : Ast.Expr.t =
  let atomic = Reify_atomic.reify_expr ~expr:Fn.id in
  Reify_prim.(Expression.reduce x ~constant ~address ~atomic ~bop ~uop)
