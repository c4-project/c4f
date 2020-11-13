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
    Heap.Monad.Monadic.return (Constant.convert_as_bool const))

let expr_as_int (x : expr) ~(mu : mu) : int Heap.Monad.t =
  Heap.Monad.Let_syntax.(
    let%bind const = mu x in
    Heap.Monad.Monadic.return (Constant.convert_as_int const))

let logical_op_sv : Op.Binary.Logical.t -> bool = function
  | And ->
      false
  | Or ->
      true

let eval_logical (op : Op.Binary.Logical.t) (l : expr) (r : expr) ~(mu : mu)
    : Constant.t Heap.Monad.t =
  Heap.Monad.Let_syntax.(
    let%bind l_value = expr_as_bool ~mu l in
    let short_value = logical_op_sv op in
    if Bool.equal l_value short_value then
      Heap.Monad.return (Constant.bool l_value)
    else mu r)

let bool_rel_op_sem : Op.Binary.Rel.t -> bool -> bool -> bool = function
  | Eq ->
      Bool.( = )
  | Ne ->
      Bool.( <> )
  | Gt ->
      Bool.( > )
  | Ge ->
      Bool.( >= )
  | Le ->
      Bool.( <= )
  | Lt ->
      Bool.( < )

let int_rel_op_sem : Op.Binary.Rel.t -> int -> int -> bool = function
  | Eq ->
      Int.( = )
  | Ne ->
      Int.( <> )
  | Gt ->
      Int.( > )
  | Ge ->
      Int.( >= )
  | Le ->
      Int.( <= )
  | Lt ->
      Int.( < )

let rel_op_sem (op : Op.Binary.Rel.t) (l : Constant.t) (r : Constant.t) :
    bool Or_error.t =
  match (l, r) with
  | Bool lb, Bool rb ->
      Ok (bool_rel_op_sem op lb rb)
  | Int li, Int ri ->
      Ok (int_rel_op_sem op li ri)
  | _ ->
      Or_error.error_s
        [%message
          "types of relational inputs don't match"
            ~left:(l : Constant.t)
            ~right:(r : Constant.t)]

let eval_rel' (op : Op.Binary.Rel.t) (l_const : Constant.t)
    (r_const : Constant.t) : Constant.t Or_error.t =
  Or_error.Let_syntax.(
    let%map v = rel_op_sem op l_const r_const in
    Constant.bool v)

let eval_rel (op : Op.Binary.Rel.t) (l : expr) (r : expr) ~(mu : mu) :
    Constant.t Heap.Monad.t =
  Heap.Monad.Let_syntax.(
    let%bind l_const = mu l in
    let%bind r_const = mu r in
    Heap.Monad.Monadic.return (eval_rel' op l_const r_const))

let eval_int_op (op : 'a) (l : expr) (r : expr)
    ~(op_sem : 'a -> int -> int -> int) ~(mu : mu) : Constant.t Heap.Monad.t
    =
  Heap.Monad.Let_syntax.(
    let%map l_int = expr_as_int ~mu l and r_int = expr_as_int ~mu r in
    let o_int = op_sem op l_int r_int in
    Constant.int o_int)

let arith_op_sem : Op.Binary.Arith.t -> int -> int -> int = function
  | Add -> (
      + )
  | Sub -> (
      - )

let bitwise_op_sem : Op.Binary.Bitwise.t -> int -> int -> int = function
  | And ->
      Int.bit_and
  | Or ->
      Int.bit_or
  | Xor ->
      Int.bit_xor

let eval_arith :
    Op.Binary.Arith.t -> expr -> expr -> mu:mu -> Constant.t Heap.Monad.t =
  eval_int_op ~op_sem:arith_op_sem

let eval_bitwise :
    Op.Binary.Bitwise.t -> expr -> expr -> mu:mu -> Constant.t Heap.Monad.t =
  eval_int_op ~op_sem:bitwise_op_sem

let eval_bop (mu : mu) :
    Op.Binary.t -> expr -> expr -> Constant.t Heap.Monad.t = function
  | Rel op ->
      eval_rel op ~mu
  | Arith op ->
      eval_arith op ~mu
  | Bitwise op ->
      eval_bitwise op ~mu
  | Logical op ->
      eval_logical op ~mu

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
    let%bind des_c = mu desired in
    let equal = Constant.equal obj_c exp_c in
    let%map () =
      if equal then Heap.Monad.store obj des_c
      else Heap.Monad.store expected obj_c
    in
    Constant.bool equal)

let atomic_fetch_op ~(op : Op.Fetch.t) ~(obj_old : Constant.t)
    ~(arg : Expression.t) : Expression.t =
  Expression.bop (Op.Fetch.to_bop op) (Expression.constant obj_old) arg

(** [eval_atomic_fetch_xchg_common obj_addr ~desired_f] handles the operation
    of loading from [obj_addr], evaluating a new value by passing its value
    to [desired_f], storing the new value back to [obj_addr], and then
    returning the old value. This body is common to both atomic_fetch and
    atomic_xchg. *)
let eval_atomic_fetch_xchg_common (obj_addr : Address.t)
    ~(desired_f : Constant.t -> Expression.t) ~(mu : mu) :
    Constant.t Heap.Monad.t =
  Heap.Monad.Let_syntax.(
    let obj = Address.deref obj_addr in
    let%bind obj_old = Heap.Monad.load obj in
    let obj_new_expr = desired_f obj_old in
    let%bind obj_new = mu obj_new_expr in
    let%map () = Heap.Monad.store obj obj_new in
    obj_old)

let eval_atomic_fetch (f : Expression.t Atomic_fetch.t) ~(mu : mu) :
    Constant.t Heap.Monad.t =
  eval_atomic_fetch_xchg_common f.obj ~mu ~desired_f:(fun obj_old ->
      let arg = f.arg in
      let op = f.op in
      atomic_fetch_op ~op ~obj_old ~arg)

let eval_atomic_load (atomic_load : Atomic_load.t) : Constant.t Heap.Monad.t
    =
  atomic_load.src |> Address.deref |> Heap.Monad.load

let eval_atomic_xchg (x : Expression.t Atomic_xchg.t) ~(mu : mu) :
    Constant.t Heap.Monad.t =
  eval_atomic_fetch_xchg_common (Atomic_xchg.obj x) ~mu ~desired_f:(fun _ ->
      Atomic_xchg.desired x)

let eval_atomic (a : Expression.t Atomic_expression.t) ~(mu : mu) :
    Constant.t Heap.Monad.t =
  Atomic_expression.reduce a ~cmpxchg:(eval_atomic_cmpxchg ~mu)
    ~fetch:(eval_atomic_fetch ~mu) ~load:eval_atomic_load
    ~xchg:(eval_atomic_xchg ~mu)

let eval_ternary ({if_; then_; else_} : Expression.t Expr_ternary.t)
    ~(mu : mu) : Constant.t Heap.Monad.t =
  Heap.Monad.(
    Let_syntax.(
      let%bind ifc = mu if_ in
      if%bind Monadic.return (Constant.convert_as_bool ifc) then mu then_
      else mu else_))

let as_constant (expr : expr) ~(env : Heap.t) : Constant.t Or_error.t =
  let rec mu (e : Expression.t) : Constant.t Heap.Monad.t =
    (* We reduce in single steps to support short-circuiting evaluation. *)
    Expression.reduce_step e ~constant:Heap.Monad.return
      ~address:Heap.Monad.load ~atomic:(eval_atomic ~mu) ~bop:(eval_bop mu)
      ~uop:(eval_uop mu) ~ternary:(eval_ternary ~mu)
  in
  Heap.Monad.run (mu expr) env
