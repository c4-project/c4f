(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Q = Base_quickcheck
module Constant = Act_c_lang.Ast_basic.Constant

let eval_guards : (bool * (unit -> 'a)) list -> 'a list =
  List.filter_map ~f:(fun (g, f) -> if g then Some (f ()) else None)

module Int_values (E : Env_types.S) :
  Act_utils.My_quickcheck.S_with_sexp with type t = Expression.t = struct
  type t = Expression.t [@@deriving sexp]

  let gen_atomic_int_load () : t Q.Generator.t =
    let module A = Atomic_load.Quickcheck_atomic_ints (E) in
    Q.Generator.map ~f:Expression.atomic_load [%quickcheck.generator: A.t]

  let gen_int_lvalue () : t Q.Generator.t =
    let module L = Lvalue_gen.Int_values (E) in
    Q.Generator.map ~f:Expression.lvalue [%quickcheck.generator: L.t]

  (** Generates the terminal integer expressions. *)
  let base_generators : t Q.Generator.t list =
    (* Use thunks here to prevent accidentally evaluating a generator that
       can't possibly work---eg, an atomic load when we don't have any
       atomic variables. *)
    eval_guards
      [ ( true
        , fun () ->
            Q.Generator.map ~f:Expression.constant
              Constant.gen_int32_constant )
      ; ( E.has_variables_of_basic_type Type.Basic.(int ~atomic:true ())
        , gen_atomic_int_load )
      ; (E.has_variables_of_basic_type Type.Basic.(int ()), gen_int_lvalue) ]

  (* let recursive_generators (_mu : t Gen.t) : t Gen.t list = [] (* No
     useful recursive expression types yet. *) ;; *)

  let quickcheck_generator : t Q.Generator.t =
    Q.Generator.union base_generators

  (* ~f:recursive_generators *)

  let quickcheck_observer : t Q.Observer.t =
    [%quickcheck.observer: Expression.t]

  (* TODO(@MattWindsor91): implement this *)
  let quickcheck_shrinker : t Q.Shrinker.t = Q.Shrinker.atomic
end

module Bool_values (E : Env_types.S) : sig
  type t = Expression.t [@@deriving sexp_of, quickcheck]
end = struct
  type t = Expression.t [@@deriving sexp]

  module Iv = Int_values (E)

  let gen_bool_lvalue () : t Q.Generator.t =
    let module L = Lvalue_gen.Bool_values (E) in
    Q.Generator.map ~f:Expression.lvalue [%quickcheck.generator: L.t]

  let gen_int_relational : t Q.Generator.t =
    let open Q.Generator.Let_syntax in
    let%bind l = [%quickcheck.generator: Iv.t] in
    let%map r = [%quickcheck.generator: Iv.t] in
    (* Only relational operation available atm. *)
    Expression.eq l r

  let gen_const : t Q.Generator.t =
    Q.Generator.map ~f:Expression.bool_lit Q.([%quickcheck.generator: bool])

  (** Generates the terminal Boolean expressions. *)
  let base_generators : t Q.Generator.t list =
    (* Use thunks here to prevent accidentally evaluating a generator that
       can't possibly work---eg, an atomic load when we don't have any
       atomic variables. *)
    eval_guards
      [ (true, fun () -> gen_const)
      ; (true, fun () -> gen_int_relational)
      ; (E.has_variables_of_basic_type Type.Basic.(bool ()), gen_bool_lvalue) ]

  let quickcheck_generator : t Q.Generator.t =
    Q.Generator.union base_generators

  let quickcheck_observer : t Q.Observer.t =
    [%quickcheck.observer: Expression.t]

  (* TODO(@MattWindsor91): implement this *)
  let quickcheck_shrinker : t Q.Shrinker.t = Q.Shrinker.atomic
end

module Bool_tautologies (E : Env_types.S_with_known_values) : sig
  type t = Expression.t [@@deriving sexp_of, quickcheck]
end = struct
  (* Define by using the bool-values quickcheck instance, but swap out the
     generator. *)
  module BV = Bool_values (E)
  include BV

  let base_generators : t Q.Generator.t list =
    (* Use thunks here to prevent accidentally evaluating a generator that
       can't possibly work---eg, an atomic load when we don't have any
       atomic variables. *)
    eval_guards
      [ (true, fun () -> Q.Generator.return (Expression.bool_lit true))
      ]

  let gen_and (mu : t Q.Generator.t) : t Q.Generator.t =
    (* Since both sides of a tautological AND must be tautological, we
       use the tautological generator twice. *)
    Q.Generator.map2 mu mu ~f:Expression.l_and

  let gen_short_or (mu : t Q.Generator.t) : t Q.Generator.t =
    (* Ensuring short-circuit by using tautological generator first. *)
    Q.Generator.map2 mu BV.quickcheck_generator ~f:Expression.l_or

  let gen_long_or (mu : t Q.Generator.t) : t Q.Generator.t =
    (* We need at least one of the terms to be tautological; this is the
       'long' version that only guarantees the RHS is. *)
    Q.Generator.map2 BV.quickcheck_generator mu ~f:Expression.l_or

  let recursive_generators (mu : t Q.Generator.t) : t Q.Generator.t list =
    eval_guards
      [ (true, fun () -> gen_and mu)
      ; (true, fun () -> gen_short_or mu)
      ; (true, fun () -> gen_long_or mu) ]

  let quickcheck_generator : t Q.Generator.t =
    Q.Generator.recursive_union base_generators
      ~f:recursive_generators
end