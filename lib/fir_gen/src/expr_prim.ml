(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

open struct
  type t = Fir.Expression.t [@@deriving sexp] (* for now *)

  type env = Fir.Env.t
end

let lift_lvalue (g : Fir.Lvalue.t Q.Generator.t) ~(env : env) :
    (Fir.Expression.t * Fir.Env.Record.t) Q.Generator.t =
  Expr_util.lift_loadlike g ~to_expr:Fir.Expression.lvalue
    ~to_var:(Accessor.get Fir.Lvalue.variable_of)
    ~env

let lift_atomic_load (g : Fir.Atomic_load.t Q.Generator.t) ~(env : env) :
    (Fir.Expression.t * Fir.Env.Record.t) Q.Generator.t =
  Expr_util.lift_loadlike g ~to_expr:Fir.Expression.atomic_load
    ~to_var:(Accessor.get Fir.Atomic_load.variable_of)
    ~env

module Int = struct
  let gen_int32 : Fir.Expression.t Q.Generator.t =
    Q.Generator.map ~f:Fir.Expression.constant Fir.Constant.gen_int32

  (* These 'let module's can't be lifted outside of their functions, because
     otherwise they'll evaluate at instantiation, and not all of them will be
     available in all environments. *)

  let gen_atomic_load (env : Fir.Env.t) :
      (t * Fir.Env.Record.t) Q.Generator.t =
    let module AL = Atomic_load.Int (struct
      let env = env
    end) in
    lift_atomic_load AL.quickcheck_generator ~env

  let gen_lvalue (env : Fir.Env.t) : (t * Fir.Env.Record.t) Q.Generator.t =
    let module LV = Lvalue.Int_values (struct
      let env = env
    end) in
    lift_lvalue LV.quickcheck_generator ~env

  let gen_load (env : env) : (t * Fir.Env.Record.t) Q.Generator.t =
    Q.Generator.union
      (Utils.My_list.eval_guards
         [ ( Expr_util.has_ints env ~is_atomic:true
           , fun () -> gen_atomic_load env )
         ; (Expr_util.has_ints env ~is_atomic:false, fun () -> gen_lvalue env)
         ] )

  let gen_plain_load (env : env) : t Q.Generator.t =
    Q.Generator.map ~f:fst (gen_load env)

  let gen (env : env) : t Q.Generator.t =
    Q.Generator.weighted_union
      (Utils.My_list.eval_guards
         [ (true, fun () -> (1.0, gen_int32))
         ; ( Fir.Env.has_vars_of_prim_type env ~prim:Int
           , fun () -> (6.0, gen_plain_load env) ) ] )
end

module Bool = struct
  let gen_const : t Q.Generator.t =
    Q.Generator.map ~f:Fir.Expression.bool_lit
      Q.([%quickcheck.generator: bool])

  let gen_atomic_load (env : env) : (t * Fir.Env.Record.t) Q.Generator.t =
    let module AL = Atomic_load.Bool (struct
      let env = env
    end) in
    lift_atomic_load AL.quickcheck_generator ~env

  let gen_lvalue (env : env) : (t * Fir.Env.Record.t) Q.Generator.t =
    let module LV = Lvalue.Bool_values (struct
      let env = env
    end) in
    lift_lvalue LV.quickcheck_generator ~env

  let gen_load (env : env) : (t * Fir.Env.Record.t) Q.Generator.t =
    Q.Generator.union
      (Utils.My_list.eval_guards
         [ ( Expr_util.has_bools env ~is_atomic:true
           , fun () -> gen_atomic_load env )
         ; ( Expr_util.has_bools env ~is_atomic:false
           , fun () -> gen_lvalue env ) ] )

  let gen_plain_load (env : env) : t Q.Generator.t =
    Q.Generator.map ~f:fst (gen_load env)

  let gen (env : env) : t Q.Generator.t =
    Q.Generator.weighted_union
      (Utils.My_list.eval_guards
         [ (true, fun () -> (4.0, gen_const))
         ; ( Fir.Env.has_vars_of_prim_type env ~prim:Bool
           , fun () -> (6.0, gen_plain_load env) ) ] )
end
