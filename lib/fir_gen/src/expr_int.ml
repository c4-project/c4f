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

  type t = Fir.Expression.t [@@deriving sexp]
end

let gen_atomic_fetch_k_nop
    (module Op_gen : Utils.My_quickcheck.S_with_sexp
      with type t = Fir.Op.Fetch.t) (k : Fir.Constant.t) (env : env)
    ~(int_const : Fir.Constant.t -> env -> t Q.Generator.t) :
    (t * Fir.Env.Record.t) Q.Generator.t =
  let module F =
    Atomic_fetch.Int
      (struct
        let env = env
      end)
      (Op_gen)
      (struct
        let sexp_of_t = sexp_of_t

        let quickcheck_observer = Fir.Expression.quickcheck_observer

        let quickcheck_shrinker = Q.Shrinker.atomic

        let quickcheck_generator = int_const k env
      end)
  in
  Expr_util.lift_loadlike [%quickcheck.generator: F.t]
    ~to_expr:Fir.Expression.atomic_fetch
    ~to_var:(Accessor.get Fir.Atomic_fetch.variable_of)
    ~env

(** [gen_atomic_fetch_zero_nop env ~int_const] generates atomic fetches that
    use zero (from [int_const]) to produce idempotent results. *)
let gen_atomic_fetch_zero_nop =
  gen_atomic_fetch_k_nop (module Fir.Op.Fetch.Gen_idem_zero_rhs) (Int 0)

(** [gen_atomic_fetch_neg1_nop env ~int_const] generates atomic fetches that
    use negative-1 (from [int_const]) to produce idempotent results. *)
let gen_atomic_fetch_neg1_nop =
  gen_atomic_fetch_k_nop
    ( module struct
      include Fir.Op.Fetch

      let quickcheck_generator = Q.Generator.return And
    end )
    (Int (-1))

let gen_atomic_fetch_nop (env : env)
    ~(int_const : Fir.Constant.t -> env -> t Q.Generator.t) :
    (t * Fir.Env.Record.t) Q.Generator.t =
  (* TODO(@MattWindsor91): also add reflexivity here, but this'll require us
     to restrict to KV variables, and this'll need a guard. *)
  Q.Generator.weighted_union
    [ (4.0, gen_atomic_fetch_zero_nop env ~int_const)
    ; (1.0, gen_atomic_fetch_neg1_nop env ~int_const) ]

let base_generators (env : env)
    ~(int_const : Fir.Constant.t -> env -> t Q.Generator.t) :
    (float * t Q.Generator.t) list =
  Utils.My_list.eval_guards
    [ (true, fun () -> (3.0, int_const (Int 0) env))
    ; (true, fun () -> (3.0, Expr_prim.Int.gen env))
      (* Prim.Int.gen subsumes Prim.Int.gen_load, so this serves to cover the
         remaining case(s) in int_loadlike. *)
    ; ( Expr_util.has_ints env ~is_atomic:true
      , fun () ->
          (2.0, Q.Generator.map ~f:fst (gen_atomic_fetch_nop env ~int_const))
      ) ]

let bitwise_bop (mu : t Q.Generator.t) : t Q.Generator.t =
  Q.Generator.(
    return Fir.Expression.bop
    <*> ( Fir.Op.Binary.Bitwise.quickcheck_generator
        >>| fun x -> Fir.Op.Binary.Bitwise x )
    <*> mu <*> mu)

let recursive_generators (mu : t Q.Generator.t) :
    (float * t Q.Generator.t) list =
  (* TODO(@MattWindsor91): find some 'safe' recursive ops. *)
  [(4.0, bitwise_bop mu)]

let gen_loadlike (env : env)
    ~(int_const : Fir.Constant.t -> env -> t Q.Generator.t) :
    (t * Fir.Env.Record.t) Q.Generator.t =
  Q.Generator.weighted_union
    (Utils.My_list.eval_guards
       [ (true, fun () -> (3.0, Expr_prim.Int.gen_load env))
       ; ( Expr_util.has_ints env ~is_atomic:true
         , fun () -> (1.0, gen_atomic_fetch_nop env ~int_const) ) ])

let gen (env : env) ~(int_const : Fir.Constant.t -> env -> t Q.Generator.t) :
    t Q.Generator.t =
  Q.Generator.weighted_recursive_union
    (base_generators env ~int_const)
    ~f:recursive_generators
