(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

open struct
  type env = Fir.Env.t

  type t = Fir.Expression.t [@@deriving sexp]
end

let lift_fetch (gen : env -> t Fir.Atomic_fetch.t Q.Generator.t) (env : env)
    : (t * Fir.Env.Record.t) Q.Generator.t =
  Expr_util.lift_loadlike (gen env) ~to_expr:Fir.Expression.atomic_fetch
    ~to_var:(Accessor.get Fir.Atomic_fetch.variable_of)
    ~env

let gen_atomic_fetch_idem (env : env)
    ~(const : Fir.Constant.t -> t Q.Generator.t) :
    (t * Fir.Env.Record.t) Q.Generator.t =
  (* TODO(@MattWindsor91): also add reflexivity here, but this'll require us
     to restrict to KV variables, and this'll need a guard. *)
  lift_fetch (Atomic_fetch.gen_int_idem ~const) env

let base_generators (env : env)
    ~(const : Fir.Constant.t -> env -> t Q.Generator.t) :
    (float * t Q.Generator.t) list =
  let const k = const k env in
  Utils.My_list.eval_guards
    [ (true, fun () -> (3.0, const (Int 0)))
    ; (true, fun () -> (3.0, Expr_prim.Int.gen env))
      (* Prim.Int.gen subsumes Prim.Int.gen_load, so this serves to cover the
         remaining case(s) in int_loadlike. *)
    ; ( Expr_util.has_ints env ~is_atomic:true
      , fun () ->
          (2.0, Q.Generator.map ~f:fst (gen_atomic_fetch_idem env ~const)) )
    ]

let bitwise_bop (mu : t Q.Generator.t) : t Q.Generator.t =
  Q.Generator.(
    return Fir.Expression.bop
    <*> ( Fir.Op.Binary.Bitwise.quickcheck_generator
        >>| fun x -> Fir.Op.Binary.Bitwise x )
    <*> mu <*> mu)

let recursive_generators (mu : t Q.Generator.t) ~(bool : t Q.Generator.t) :
    (float * t Q.Generator.t) list =
  (* TODO(@MattWindsor91): find some 'safe' recursive ops. *)
  [ (4.0, bitwise_bop mu)
  ; (2.0, Expr_util.ternary ~gen_if:bool ~gen_then:mu ~gen_else:mu) ]

let gen_loadlike (env : env)
    ~(const : Fir.Constant.t -> env -> t Q.Generator.t) :
    (t * Fir.Env.Record.t) Q.Generator.t =
  let const k = const k env in
  Q.Generator.weighted_union
    (Utils.My_list.eval_guards
       [ (true, fun () -> (3.0, Expr_prim.Int.gen_load env))
       ; ( Expr_util.has_ints env ~is_atomic:true
         , fun () -> (1.0, gen_atomic_fetch_idem env ~const) ) ] )

let gen (env : env) ~(bool : env -> t Q.Generator.t)
    ~(const : Fir.Constant.t -> env -> t Q.Generator.t) : t Q.Generator.t =
  (* TODO(@MattWindsor91): fix consts? *)
  let bool = Q.Generator.of_lazy (lazy (bool env)) in
  Q.Generator.weighted_recursive_union
    (base_generators env ~const)
    ~f:(recursive_generators ~bool)
