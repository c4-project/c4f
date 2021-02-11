(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

let has_ints (env : Fir.Env.t) ~(is_atomic : bool) : bool =
  Fir.Env.has_vars_of_basic_type env
    ~basic:(Fir.Type.Basic.int ~is_atomic ())

let has_bools (env : Fir.Env.t) ~(is_atomic : bool) : bool =
  Fir.Env.has_vars_of_basic_type env
    ~basic:(Fir.Type.Basic.bool ~is_atomic ())

let with_record (g : 'a Q.Generator.t) ~(to_var : 'a -> C4f_common.C_id.t)
    ~(env : Fir.Env.t) : ('a * Fir.Env.Record.t) Q.Generator.t =
  Q.Generator.filter_map g ~f:(fun x ->
      Option.(x |> to_var |> Map.find env >>| fun r -> (x, r)) )

let lift_loadlike (g : 'a Q.Generator.t) ~(to_expr : 'a -> Fir.Expression.t)
    ~(to_var : 'a -> Common.C_id.t) ~(env : Fir.Env.t) :
    (Fir.Expression.t * Fir.Env.Record.t) Q.Generator.t =
  Q.Generator.map
    ~f:(fun (l, r) -> (to_expr l, r))
    (with_record g ~to_var ~env)

let gen_kv_refl (type v a)
    ~(gen_op : v -> Fir.Expression.t -> a Q.Generator.t)
    ~(gen_load : (v * Fir.Env.Record.t) Q.Generator.t) : a Q.Generator.t =
  Q.Generator.Let_syntax.(
    let%bind v, kv =
      Q.Generator.filter_map gen_load ~f:(fun (l, r) ->
          Option.Let_syntax.(
            let%map kv = Accessor.get_option Fir.Env.Record.known_value r in
            (l, Fir.Expression.constant kv)) )
    in
    gen_op v kv)

let div (k : int) (x : 'a Q.Generator.t) : 'a Q.Generator.t =
  Q.Generator.(
    Let_syntax.(
      let%bind size = size in
      with_size ~size:(size / k) x))

let half (x : 'a Q.Generator.t) : 'a Q.Generator.t = div 2 x

let ternary ~(gen_if : Fir.Expression.t Q.Generator.t)
    ~(gen_then : Fir.Expression.t Q.Generator.t)
    ~(gen_else : Fir.Expression.t Q.Generator.t) :
    Fir.Expression.t Q.Generator.t =
  let gen_if = div 3 gen_if in
  let gen_then = div 3 gen_then in
  let gen_else = div 3 gen_else in
  Q.Generator.map ~f:Fir.Expression.ternary
    (Fir.Expr_ternary.quickcheck_generator_ite ~gen_if ~gen_then ~gen_else)
