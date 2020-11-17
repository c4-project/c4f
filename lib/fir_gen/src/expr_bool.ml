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

(** [int_relational ~gen_int] generates arbitrary relational expressions over
    [gen_int]. *)
let int_relational ~(gen_int : t Q.Generator.t) : t Q.Generator.t =
  Q.Generator.(
    Let_syntax.(
      let%bind size = size in
      let g = with_size gen_int ~size:(size / 2) in
      let%map l = g
      and r = g
      and op = [%quickcheck.generator: Fir.Op.Binary.Rel.t] in
      Fir.Expression.bop (Fir.Op.Binary.Rel op) l r))

(** Generates the terminal Boolean expressions. *)
let base_generators (env : env) ~(int : env -> t Q.Generator.t) :
    (float * t Q.Generator.t) list =
  (* Use thunks here to prevent accidentally evaluating a generator that
     can't possibly work---eg, an atomic load when we don't have any atomic
     variables. *)
  Utils.My_list.eval_guards
    [ (true, fun () -> (5.0, Expr_prim.Bool.gen env))
    ; (true, fun () -> (5.0, int_relational ~gen_int:(int env))) ]

let recursive_generators (mu : t Q.Generator.t) :
    (float * t Q.Generator.t) list =
  [ (3.0, Q.Generator.map2 mu mu ~f:Fir.Expression.l_and)
  ; (3.0, Q.Generator.map2 mu mu ~f:Fir.Expression.l_or)
  ; (2.0, Q.Generator.map mu ~f:Fir.Expression.l_not) ]

let gen (env : env) ~(int : env -> t Q.Generator.t) : t Q.Generator.t =
  Q.Generator.weighted_recursive_union
    (base_generators env ~int)
    ~f:recursive_generators
