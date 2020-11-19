(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Operand_set = struct
  type t =
    | One of Fir.Expression.t
    | Two of Fir.Expression.t * Fir.Expression.t

  let take_one : t -> Fir.Expression.t Q.Generator.t =
    Q.Generator.(function One x -> return x | Two (l, r) -> of_list [l; r])

  let take_two : t -> (Fir.Expression.t * Fir.Expression.t) Q.Generator.t =
    Q.Generator.(
      function
      | One x -> return (x, x) | Two (l, r) -> of_list [(l, r); (r, l)])
end

let bop_of_const
    (f : Fir.Expression.t -> Fir.Expression.t -> Fir.Expression.t)
    (x : Fir.Expression.t) ~(dir : Fir.Op_rule.In.Dir.t)
    ~(k : Fir.Constant.t)
    ~(lift_k : Fir.Constant.t -> Fir.Expression.t Q.Generator.t) :
    Fir.Expression.t Q.Generator.t =
  Q.Generator.(
    lift_k k >>| fun ke -> match dir with Left -> f ke x | Right -> f x ke)

let bop_of_rule (rule : Fir.Op_rule.In.t) (ops : Operand_set.t)
    ~(op : Fir.Op.Binary.t)
    ~(lift_k : Fir.Constant.t -> Fir.Expression.t Q.Generator.t) :
    Fir.Expression.t Q.Generator.t =
  let f = Fir.Expression.bop op in
  Q.Generator.(
    match rule with
    | Refl ->
        ops |> Operand_set.take_two >>| fun (l, r) -> f l r
    | Const (dir, k) ->
        ops |> Operand_set.take_one >>= bop_of_const f ~dir ~k ~lift_k)

let rulesi :
    ( (Fir.Op.Binary.t * int) * 'a -> Fir.Op_rule.t -> Fir.Op_rule.t
    , 'a -> Fir.Op.Binary.t -> Fir.Op.Binary.t
    , [< many_getter] )
    Accessor.t =
  Accessor.(
    many_getteri (fun op ->
        op |> Fir.Op.Binary.rules
        |> Base.List.mapi ~f:(fun i r -> Many_getter.access ((op, i), r))
        |> Many_getter.of_list))

let in_rulesi (out : Fir.Op_rule.Out.t) :
    ( (Fir.Op.Binary.t * int) * 'a -> Fir.Op_rule.In.t -> Fir.Op_rule.In.t
    , 'a -> Fir.Op.Binary.t -> Fir.Op.Binary.t
    , [< many_getter] )
    Accessor.t =
  rulesi @> Fir.Op_rule.single_in_matching out

let bop_of_indexed_rule
    ((ix, rule) : (('a * int) * unit) Accessor.Index.t * Fir.Op_rule.In.t)
    ~(operands : Operand_set.t)
    ~(lift_k : Fir.Constant.t -> Fir.Expression.t Q.Generator.t) :
    Fir.Expression.t Q.Generator.t =
  let op, _ = Accessor.Index.hd ix in
  bop_of_rule rule operands ~op ~lift_k

let basic_lift_k (k : Fir.Constant.t) : Fir.Expression.t Q.Generator.t =
  Q.Generator.return (Fir.Expression.constant k)

type bop_gen =
     (Fir.Constant.t -> Fir.Expression.t Q.Generator.t)
  -> Operand_set.t
  -> Fir.Expression.t Q.Generator.t

let bop_with_output ?(ops : Fir.Op.Binary.t list = Fir.Op.Binary.all)
    (out : Fir.Op_rule.Out.t) : bop_gen option =
  (* TODO(@MattWindsor91): I suspect this is woefully inefficient. *)
  match Accessor.(to_listi (List.each @> in_rulesi out) ops) with
  | [] ->
      None
  | ins ->
      Some
        (fun lift_k operands ->
          Q.Generator.(of_list ins >>= bop_of_indexed_rule ~operands ~lift_k))
