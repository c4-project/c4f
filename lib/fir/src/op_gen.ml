(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

open struct
  module A = Accessor_base
  module Q = Base_quickcheck
end

module Operand_set = struct
  type t = One of Expression.t | Two of Expression.t * Expression.t

  (* Getters that can retrieve operands in any order, reusing them if
     necessary. *)

  let take_one : ('a, Expression.t, t, [< A.many_getter]) A.Simple.t =
    [%accessor
      A.(
        many_getter (function
          | Two (l, r) ->
              Many_getter.(access l @ access r)
          | One l ->
              Many_getter.access l))]

  let take_two :
      ('a, Expression.t * Expression.t, t, [< A.many_getter]) A.Simple.t =
    [%accessor
      A.(
        many_getter (function
          | Two (l, r) ->
              Many_getter.(access (l, r) @ access (r, l))
          | One l ->
              Many_getter.access (l, l)))]
end

let bop_of_refl (f : Expression.t -> Expression.t -> Expression.t) :
    ( 'a
    , Expression.t
    , Expression.t * Expression.t
    , [< A.many_getter] )
    A.Simple.t =
  A.(getter (fun (l, r) -> f l r))

let bop_of_const' (f : Expression.t -> Expression.t -> Expression.t)
    (x : Expression.t) ~(dir : Op_rule.In.Dir.t) ~(k : Expression.t) =
  A.Many_getter.(
    match dir with Left -> access (f k x) | Right -> access (f x k))

let bop_of_const (f : Expression.t -> Expression.t -> Expression.t)
    ~(dir : Op_rule.In.Dir.t) ~(k : Constant.t) :
    ('a, Expression.t, Expression.t, [< A.many_getter]) A.Simple.t =
  A.many_getter (bop_of_const' f ~dir ~k:(Expression.constant k))

let bop_of_rule (rule : Op_rule.In.t) ~(op : Op.Binary.t) :
    ('a, Expression.t, Operand_set.t, [< A.many_getter]) A.Simple.t =
  let f = Expression.bop op in
  match rule with
  | Refl ->
      A.(Operand_set.take_two @> bop_of_refl f)
  | Const (dir, k) ->
      A.(Operand_set.take_one @> bop_of_const f ~dir ~k)

let rulesi (type t) (module M : Op_types.S_binary with type t = t) :
    ( (t * int) * 'a -> Op_rule.t -> Op_rule.t
    , 'a -> t -> t
    , [< A.many_getter] )
    A.t =
  A.(
    many_getteri (fun op ->
        op |> M.rules
        |> Base.List.mapi ~f:(fun i r -> Many_getter.access ((op, i), r))
        |> Many_getter.of_list))

let in_rulesi (type t) (module M : Op_types.S_binary with type t = t)
    (out : Op_rule.Out.t) :
    ( (t * int) * 'a -> Op_rule.In.t -> Op_rule.In.t
    , 'a -> t -> t
    , [< A.many_getter] )
    A.t =
  A.(rulesi (module M) @> Op_rule.single_in_matching out)

let bop_of_indexed_rule
    ((ix, rule) : (('a * int) * unit) A.Index.t * Op_rule.In.t)
    ~(operands : Operand_set.t) ~(promote : 'a -> Op.Binary.t) :
    Expression.t Q.Generator.t =
  let op, _ = A.Index.hd ix in
  Q.Generator.of_list
    (A.to_list (bop_of_rule rule ~op:(promote op)) operands)

let bop (type t) (module M : Op_types.S_binary with type t = t)
    (operands : Operand_set.t) ~(promote : t -> Op.Binary.t)
    ~(out : Op_rule.Out.t) : Expression.t Q.Generator.t =
  (* TODO(@MattWindsor91): I suspect this is woefully inefficient. *)
  let ins = A.(to_listi (List.each @> in_rulesi (module M) out)) M.all in
  Q.Generator.(of_list ins >>= bop_of_indexed_rule ~promote ~operands)
