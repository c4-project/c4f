(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type expr = Expression.t

type mu = expr -> Constant.t Heap.Monad.t

let expr_as_bool (x : expr) ~(mu : mu) : bool Heap.Monad.t =
  Heap.Monad.Let_syntax.(
    let%bind const = mu x in
    Heap.Monad.Monadic.return (Constant.as_bool const))

let expr_as_int (x : expr) ~(mu : mu) : int Heap.Monad.t =
  Heap.Monad.Let_syntax.(
    let%bind const = mu x in
    Heap.Monad.Monadic.return (Constant.as_int const))

let eval_logical (l : expr) (r : expr) ~(short_value : bool) ~(mu : mu) :
    Constant.t Heap.Monad.t =
  Heap.Monad.Let_syntax.(
    let%bind l_value = expr_as_bool ~mu l in
    if Bool.equal l_value short_value then
      Heap.Monad.return (Constant.bool l_value)
    else mu r)

let eval_land = eval_logical ~short_value:false

let eval_lor = eval_logical ~short_value:true

let eval_eq (l : expr) (r : expr) ~(mu : mu) : Constant.t Heap.Monad.t =
  Heap.Monad.Let_syntax.(
    let%bind l_const = mu l in
    let%bind r_const = mu r in
    if Comparable.lift [%equal: Type.t] ~f:Constant.type_of l_const r_const
    then
      Heap.Monad.return
        (Constant.bool ([%equal: Constant.t] l_const r_const))
    else
      Heap.Monad.Monadic.return
        (Or_error.error_s
           [%message
             "eq: types of constants are incompatible"
               ~left:(l_const : Constant.t)
               ~right:(r_const : Constant.t)]))

let eval_arith (op : Op.Binary.Arith.t) ~(mu : mu) (l : expr) (r : expr) :
    Constant.t Heap.Monad.t =
  Heap.Monad.Let_syntax.(
    let%map l_int = expr_as_int ~mu l and r_int = expr_as_int ~mu r in
    let o_int =
      match op with Add -> l_int + r_int | Sub -> l_int - r_int
    in
    Constant.int o_int)

let eval_bop (mu : mu) :
    Op.Binary.t -> expr -> expr -> Constant.t Heap.Monad.t = function
  | Arith op ->
      eval_arith op ~mu
  | Logical And ->
      eval_land ~mu
  | Logical Or ->
      eval_lor ~mu
  | Eq ->
      eval_eq ~mu

let eval_lnot (x : expr) ~(mu : mu) : Constant.t Heap.Monad.t =
  Heap.Monad.(x |> expr_as_bool ~mu >>| not >>| Constant.bool)

let eval_uop (mu : mu) : Op.Unary.t -> expr -> Constant.t Heap.Monad.t =
  function
  | L_not ->
      eval_lnot ~mu

(* We don't specifically handle memory order here, since we assume that the
   known-values environment refers to things that are already fully
   propagated through memory. *)

let eval_atomic_cmpxchg (c : Expression.t Atomic_cmpxchg.t) ~(mu : mu) :
    Constant.t Heap.Monad.t =
  Heap.Monad.Let_syntax.(
    let obj = Address.deref (Atomic_cmpxchg.obj c) in
    let expected = Address.deref (Atomic_cmpxchg.expected c) in
    let desired = Atomic_cmpxchg.desired c in
    let%bind obj_c = Heap.Monad.load obj in
    let%bind exp_c = Heap.Monad.load expected in
    let equal = Constant.equal obj_c exp_c in
    let%map () =
      if equal then desired |> mu >>= Heap.Monad.store obj
      else Heap.Monad.store expected obj_c
    in
    Constant.bool equal)

let atomic_fetch_op ~(op : Op.Fetch.t) ~(obj_old : Constant.t)
    ~(arg : Expression.t) : Expression.t =
  Expression.bop (Op.Fetch.to_bop op) (Expression.constant obj_old) arg

let eval_atomic_fetch (f : Expression.t Atomic_fetch.t) ~(mu : mu) :
    Constant.t Heap.Monad.t =
  Heap.Monad.Let_syntax.(
    let obj = Address.deref (Atomic_fetch.obj f) in
    let arg = Atomic_fetch.arg f in
    let op = Atomic_fetch.op f in
    let%bind obj_old = Heap.Monad.load obj in
    let obj_new_expr = atomic_fetch_op ~op ~obj_old ~arg in
    let%bind obj_new = mu obj_new_expr in
    let%map () = Heap.Monad.store obj obj_new in
    obj_old)

let eval_atomic_load (atomic_load : Atomic_load.t) : Constant.t Heap.Monad.t
    =
  atomic_load |> Atomic_load.src |> Address.deref |> Heap.Monad.load

let eval_atomic (a : Expression.t Atomic_expression.t) ~(mu : mu) :
    Constant.t Heap.Monad.t =
  Atomic_expression.reduce a ~cmpxchg:(eval_atomic_cmpxchg ~mu)
    ~fetch:(eval_atomic_fetch ~mu) ~load:eval_atomic_load

let as_constant (expr : expr) ~(env : Heap.t) : Constant.t Or_error.t =
  let rec mu : Expression.t -> Constant.t Heap.Monad.t =
    (* We reduce in single steps to support short-circuiting evaluation. *)
    Expression.reduce_step ~constant:Heap.Monad.return
      ~address:Heap.Monad.load
      ~atomic:(fun o -> eval_atomic ~mu o)
      ~bop:(fun o -> eval_bop mu o)
      ~uop:(fun o -> eval_uop mu o)
  in
  Heap.Monad.run (mu expr) env
