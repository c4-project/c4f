(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Import

open struct
  type t = Fir.Expression.t Fir.Atomic_fetch.t

  type env = Fir.Env.t

  type const_f = Fir.Constant.t -> Fir.Expression.t Q.Generator.t
end

module type S = sig
  type t = Fir.Expression.t Fir.Atomic_fetch.t
  [@@deriving sexp_of, quickcheck]
end

module Int
    (Obj : Fir.Env_types.S)
    (O : Utils.My_quickcheck.S_with_sexp with type t := Fir.Op.Fetch.t)
    (Arg : Utils.My_quickcheck.S_with_sexp with type t := Fir.Expression.t) :
  S =
  Fir.Atomic_fetch.Quickcheck_generic
    (Address.Atomic_int_pointers (Obj)) (O)
    (struct
      type t = Fir.Expression.t

      include Arg
    end)

let gen_k_idem
    (module Op_gen : Utils.My_quickcheck.S_with_sexp
      with type t = Fir.Op.Fetch.t ) (k : Fir.Constant.t) (env : env)
    ~(const : const_f) : t Q.Generator.t =
  (* TODO(@MattWindsor91): perhaps generate these from the rule tables? *)
  let module F =
    Int
      (struct
        let env = env
      end)
      (Op_gen)
      (struct
        let sexp_of_t = Fir.Expression.sexp_of_t

        let quickcheck_observer = Fir.Expression.quickcheck_observer

        let quickcheck_shrinker = Q.Shrinker.atomic

        let quickcheck_generator = const k
      end)
  in
  [%quickcheck.generator: F.t]

(** [gen_atomic_fetch_zero_idem env ~const] generates atomic fetches that use
    zero (from [const]) to produce idempotent results. *)
let gen_zero_idem : env -> const:const_f -> t Q.Generator.t =
  gen_k_idem (module Fir.Op.Fetch.Gen_idem_zero_rhs) (Int 0)

(** [gen_atomic_fetch_neg1_idem env ~const] generates atomic fetches that use
    negative-1 (from [const]) to produce idempotent results. *)
let gen_neg1_idem : env -> const:const_f -> t Q.Generator.t =
  gen_k_idem
    ( module struct
      include Fir.Op.Fetch

      let quickcheck_generator = Q.Generator.return `And
    end )
    (Int (-1))

let gen_refl_op_base (obj : Fir.Address.t) (arg : Fir.Expression.t) :
    Fir.Expression.t Fir.Atomic_fetch.t Q.Generator.t =
  Q.Generator.Let_syntax.(
    let%bind op = Fir.Op.Fetch.Gen_idem_refl.quickcheck_generator in
    (* 'all values are permitted':
       https://en.cppreference.com/w/c/atomic/atomic_fetch_add etc. *)
    let%map mo = Fir.Mem_order.quickcheck_generator in
    Fir.Atomic_fetch.make ~obj ~arg ~mo ~op)

let gen_int_refl_idem (env : env) : t Q.Generator.t =
  let module A = Address.Atomic_int_pointers (struct
    let env = env
  end) in
  let gen_load =
    Expr_util.with_record A.quickcheck_generator
      ~to_var:(Accessor.get Fir.Address.variable_of)
      ~env
  in
  Expr_util.gen_kv_refl ~gen_load ~gen_op:gen_refl_op_base

let has_known_ints (env : env) : bool =
  (* TODO(@MattWindsor91): more efficient? *)
  Fir.Env.(
    has_vars_of_basic_type
      ~basic:(Fir.Type.Basic.int ~is_atomic:true ())
      (filter_to_known_values env))

let gen_int_idem (env : env) ~(const : const_f) : t Q.Generator.t =
  Q.Generator.weighted_union
  @@ Utils.My_list.eval_guards
       [ (true, fun () -> (4.0, gen_zero_idem env ~const))
       ; (true, fun () -> (1.0, gen_neg1_idem env ~const))
       ; (has_known_ints env, fun () -> (2.0, gen_int_refl_idem env)) ]
