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

  (* Getters that can retrieve operands in any order, reusing them if
     necessary. *)

  let take_one : ('a, Fir.Expression.t, t, [< many_getter]) Accessor.Simple.t
      =
    [%accessor
      Accessor.(
        many_getter (function
          | Two (l, r) ->
              Many_getter.(access l @ access r)
          | One l ->
              Many_getter.access l))]

  let take_two :
      ( 'a
      , Fir.Expression.t * Fir.Expression.t
      , t
      , [< many_getter] )
      Accessor.Simple.t =
    [%accessor
      Accessor.(
        many_getter (function
          | Two (l, r) ->
              Many_getter.(access (l, r) @ access (r, l))
          | One l ->
              Many_getter.access (l, l)))]
end

let bop_of_refl
    (f : Fir.Expression.t -> Fir.Expression.t -> Fir.Expression.t) :
    ( 'a
    , Fir.Expression.t
    , Fir.Expression.t * Fir.Expression.t
    , [< many_getter] )
    Accessor.Simple.t =
  Accessor.getter (fun (l, r) -> f l r)

let bop_of_const'
    (f : Fir.Expression.t -> Fir.Expression.t -> Fir.Expression.t)
    (x : Fir.Expression.t) ~(dir : Fir.Op_rule.In.Dir.t)
    ~(k : Fir.Expression.t) =
  Accessor.Many_getter.(
    match dir with Left -> access (f k x) | Right -> access (f x k))

let bop_of_const
    (f : Fir.Expression.t -> Fir.Expression.t -> Fir.Expression.t)
    ~(dir : Fir.Op_rule.In.Dir.t) ~(k : Fir.Constant.t) :
    ( 'a
    , Fir.Expression.t
    , Fir.Expression.t
    , [< many_getter] )
    Accessor.Simple.t =
  Accessor.many_getter (bop_of_const' f ~dir ~k:(Fir.Expression.constant k))

let bop_of_rule (rule : Fir.Op_rule.In.t) ~(op : Fir.Op.Binary.t) :
    ('a, Fir.Expression.t, Operand_set.t, [< many_getter]) Accessor.Simple.t
    =
  let f = Fir.Expression.bop op in
  match rule with
  | Refl ->
      Operand_set.take_two @> bop_of_refl f
  | Const (dir, k) ->
      Operand_set.take_one @> bop_of_const f ~dir ~k

let rulesi (type t) (module M : Fir.Op_types.S_binary with type t = t) :
    ( (t * int) * 'a -> Fir.Op_rule.t -> Fir.Op_rule.t
    , 'a -> t -> t
    , [< many_getter] )
    Accessor.t =
  Accessor.(
    many_getteri (fun op ->
        op |> M.rules
        |> Base.List.mapi ~f:(fun i r -> Many_getter.access ((op, i), r))
        |> Many_getter.of_list))

let in_rulesi (type t) (module M : Fir.Op_types.S_binary with type t = t)
    (out : Fir.Op_rule.Out.t) :
    ( (t * int) * 'a -> Fir.Op_rule.In.t -> Fir.Op_rule.In.t
    , 'a -> t -> t
    , [< many_getter] )
    Accessor.t =
  rulesi (module M) @> Fir.Op_rule.single_in_matching out

let bop_of_indexed_rule
    ((ix, rule) : (('a * int) * unit) Accessor.Index.t * Fir.Op_rule.In.t)
    ~(operands : Operand_set.t) ~(promote : 'a -> Fir.Op.Binary.t) :
    Fir.Expression.t Q.Generator.t =
  let op, _ = Accessor.Index.hd ix in
  Q.Generator.of_list operands.@*(bop_of_rule rule ~op:(promote op))

let bop (type t) (module M : Fir.Op_types.S_binary with type t = t)
    ~(promote : t -> Fir.Op.Binary.t) ~(out : Fir.Op_rule.Out.t) :
    (Operand_set.t -> Fir.Expression.t Q.Generator.t) option =
  (* TODO(@MattWindsor91): I suspect this is woefully inefficient. *)
  match
    Accessor.(to_listi (List.each @> in_rulesi (module M) out) M.all)
  with
  | [] ->
      None
  | ins ->
      Some
        (fun operands ->
          Q.Generator.(
            of_list ins >>= bop_of_indexed_rule ~promote ~operands))
