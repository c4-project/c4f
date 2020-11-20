(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

open struct
  type env = Fir.Env.t

  type t = Fir.Expression.t
end

(** Generator set for a particular type. *)
type gctx =
  { arb: t Q.Generator.t
  ; load: (t * Fir.Env.Record.t) Q.Generator.t option
  ; kbop: Op.bop_gen option
  ; ibop: Op.bop_gen option }

(** [kbop k in_type] tries to make a generator for binary operations that
    have operands of value type [in_type], but result in [k]. *)
let kbop (k : Fir.Constant.t) ~(in_type : Fir.Type.Prim.t) :
    Op.bop_gen option =
  (* TODO(@MattWindsor91): make sure the type is right. *)
  Op.bop_with_output
    ~ops:(Fir.Op.Binary.of_input_prim_type in_type)
    (Const k)

(** [ibop t] tries to make a generator for binary operations that take inputs
    of type [t] and are idempotent. *)
let ibop (t : Fir.Type.Prim.t) : Op.bop_gen option =
  (* There is always at least one Idem operator: for example, bitwise OR for
     integers and logical OR for Booleans. *)
  Op.bop_with_output ~ops:(Fir.Op.Binary.of_input_prim_type t) Idem

(** [arb_bop ~gen_arb ~bop] generates binary operations of the form [x op x],
    [x op k2], or [k2 op x], where [x] is an arbitrary expression generated
    by [gen_arb], [k2] is a specific constant, and the resulting operation is
    known to produce the wanted constant. *)
let arb_bop ~(gen_arb : t Q.Generator.t)
    ~(k_mu : Fir.Constant.t -> t Q.Generator.t) ~(bop : Op.bop_gen) :
    t Q.Generator.t =
  Q.Generator.Let_syntax.(
    let%bind p = Expr_util.half gen_arb in
    bop (Fn.compose Expr_util.half k_mu) (One p))

(** [kv_bop ~gen_load ~bop] generates binary operations of the form [x op y],
    in which one of [x] and [y] is a variable, the other is its known value,
    and the operation is statically known to produce the wanted constant. *)
let kv_bop ~(gen_load : (t * Fir.Env.Record.t) Q.Generator.t)
    ~(k_mu : Fir.Constant.t -> t Q.Generator.t) ~(bop : Op.bop_gen) :
    t Q.Generator.t =
  Expr_util.gen_kv_refl ~gen_load ~gen_op:(fun l r ->
      bop (Fn.compose Expr_util.half k_mu) (Two (l, r)))

let base_generators (k : Fir.Constant.t) : (float * t Q.Generator.t) list =
  (* TODO(@MattWindsor91): known values of kv_env variables *)
  [(1.0, Q.Generator.return (Fir.Expression.constant k))]

(** [kbop_generators gctx] produces a weighted list of all generators over a
    particular type's generator context that produce binary operations
    guaranteed to result in the given constant given semi-arbitrary data. *)
let kbop_generators ({kbop; arb; load; _} : gctx)
    ~(k_mu : Fir.Constant.t -> t Q.Generator.t) :
    (float * t Q.Generator.t) list =
  List.filter_opt
    [ Option.map kbop ~f:(fun bop -> (3.0, arb_bop ~k_mu ~gen_arb:arb ~bop))
    ; Option.map2 kbop load ~f:(fun bop gen_load ->
          (5.0, kv_bop ~k_mu ~gen_load ~bop)) ]

let ternaries ~(mu : t Q.Generator.t)
    ~(k_mu : Fir.Constant.t -> t Q.Generator.t) ~(arb : t Q.Generator.t) :
    t Q.Generator.t =
  Q.Generator.union
    [ Expr_util.ternary ~gen_if:(k_mu (Bool true)) ~gen_then:mu ~gen_else:arb
    ; Expr_util.ternary ~gen_if:(k_mu (Bool false)) ~gen_then:arb
        ~gen_else:mu ]

let bool_specifics (this_val : bool)
    ~(k_mu : Fir.Constant.t -> t Q.Generator.t) : t Q.Generator.t =
  (* TODO(@MattWindsor91): consider other possibilities here? *)
  Q.Generator.map (k_mu (Bool (not this_val))) ~f:Fir.Expression.l_not

(** [recursive_generators k mu ~k_mu ~int_ctx ~bool_ctx ~k_gctx] contains the
    various constant-value expression generators that either recurse directly
    over the same generator ([mu]), recurse into another constant generator
    ([k_mu k] where [k] is not the constant being generated here), or summon
    other expression generators which may or may not recurse back into this
    constant generator ([int_ctx], which has integer-typed generators;
    [bool_ctx], which has bool-typed generators; and [k_gctx], which has
    generators typed the same as the constant in question). *)
let recursive_generators (k : Fir.Constant.t) (mu : t Q.Generator.t)
    ~(k_mu : Fir.Constant.t -> t Q.Generator.t option) ~(int_gctx : gctx)
    ~(bool_gctx : gctx) ~(k_gctx : gctx) : (float * t Q.Generator.t) list =
  let k_mu k = Option.value ~default:mu (k_mu k) in
  kbop_generators ~k_mu int_gctx
  @ kbop_generators ~k_mu bool_gctx
  @ List.filter_opt
      [ Some (4.0, ternaries ~mu ~k_mu ~arb:k_gctx.arb)
      ; Option.map
          (Result.ok (Fir.Constant.as_bool k))
          ~f:(fun b -> (3.0, bool_specifics b ~k_mu))
      ; Option.map k_gctx.ibop ~f:(fun bop ->
            ( 3.0
            , Q.Generator.Let_syntax.(
                let%bind x = Expr_util.half mu and y = Expr_util.half mu in
                bop (Fn.compose Expr_util.half k_mu) (Two (x, y))) )) ]

let rec_on_other_constant (k : Fir.Constant.t) ~(this_k : Fir.Constant.t)
    ~(mu : Fir.Constant.t -> t Q.Generator.t) : t Q.Generator.t option =
  if Fir.Constant.(k = this_k) then None
  else
    Some
      Q.Generator.(
        (* The laziness has to cover calling [mu] here; else, there's an
           infinite loop of constructing [rec_on_other_constant]. *)
        of_lazy
        @@ lazy
             Let_syntax.(
               let%bind size = size in
               let size = if size = 0 then 0 else size - 1 in
               with_size ~size (mu k)))

let gen (k : Fir.Constant.t) (env : env) ~(int : env -> t Q.Generator.t)
    ~(bool : env -> t Q.Generator.t)
    ~(int_load : env -> (t * Fir.Env.Record.t) Q.Generator.t)
    ~(bool_load : env -> (t * Fir.Env.Record.t) Q.Generator.t) :
    t Q.Generator.t =
  let kv_env = Fir.Env.filter_to_known_values env in
  let has_ints = Fir.Env.has_vars_of_prim_type kv_env ~prim:Int in
  let has_bools = Fir.Env.has_vars_of_prim_type kv_env ~prim:Bool in
  let int = Q.Generator.of_lazy (lazy (int kv_env)) in
  let bool = Q.Generator.of_lazy (lazy (bool kv_env)) in
  let int_load =
    Option.some_if has_ints (Q.Generator.of_lazy (lazy (int_load kv_env)))
  in
  let bool_load =
    Option.some_if has_bools (Q.Generator.of_lazy (lazy (bool_load kv_env)))
  in
  (* The recursion in this generator is slightly weird: we have the normal
     recursion given to us by Base_quickcheck (called [mu] in this file), and
     this outer recursion that is free on the constant (called [k_mu]).

     Calling into [k_mu] is relatively expensive (we think - we don't have a
     good profiler at time of writing), so we try to avoid doing it: we set
     up [k_mu] here and in [recursive_generators] so that it delegates to
     [mu] where possible. *)
  let rec k_mu (k : Fir.Constant.t) =
    let int_gctx =
      {arb= int; load= int_load; kbop= kbop k ~in_type:Int; ibop= ibop Int}
    in
    let bool_gctx =
      { arb= bool
      ; load= bool_load
      ; kbop= kbop k ~in_type:Bool
      ; ibop= ibop Bool }
    in
    let k_gctx =
      match Fir.Constant.prim_type_of k with
      | Int ->
          int_gctx
      | Bool ->
          bool_gctx
    in
    let k_mu = rec_on_other_constant ~this_k:k ~mu:k_mu in
    Q.Generator.weighted_recursive_union (base_generators k)
      ~f:(recursive_generators k ~int_gctx ~bool_gctx ~k_gctx ~k_mu)
  in
  k_mu k
