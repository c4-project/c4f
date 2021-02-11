(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module type S =
  Utils.My_quickcheck.S_with_sexp with type t = Fir.Expression.t

open struct
  type env = Fir.Env.t

  type t = Fir.Expression.t [@@deriving sexp]
end

let bool_load (env : env) : (t * Fir.Env.Record.t) Q.Generator.t =
  (* We don't have any non-primitive load-like booleans yet. *)
  Expr_prim.Bool.gen_load env

let rec const (k : Fir.Constant.t) (env : env) : t Q.Generator.t =
  Expr_const.gen k env ~int_load ~bool_load ~int ~bool

and int_load (env : env) : (t * Fir.Env.Record.t) Q.Generator.t =
  Expr_int.gen_loadlike env ~const

and int (env : env) : t Q.Generator.t = Expr_int.gen env ~bool ~const

and bool (env : env) : t Q.Generator.t = Expr_bool.gen env ~int

let tautology : env -> Fir.Expression.t Q.Generator.t = const (Bool true)

let falsehood : env -> Fir.Expression.t Q.Generator.t = const (Bool false)

module Int_zeroes (E : Fir.Env_types.S) = struct
  type t = Fir.Expression.t [@@deriving sexp]

  let quickcheck_generator : t Q.Generator.t = const (Int 0) E.env

  let quickcheck_observer : t Q.Observer.t =
    [%quickcheck.observer: Fir.Expression.t]

  (* TODO(@MattWindsor91): implement this *)
  let quickcheck_shrinker : t Q.Shrinker.t = Q.Shrinker.atomic
end

module Int_values (E : Fir.Env_types.S) = struct
  type t = Fir.Expression.t [@@deriving sexp]

  let quickcheck_generator : t Q.Generator.t = int E.env

  let quickcheck_observer : t Q.Observer.t =
    [%quickcheck.observer: Fir.Expression.t]

  (* TODO(@MattWindsor91): implement this *)
  let quickcheck_shrinker : t Q.Shrinker.t = Q.Shrinker.atomic
end

module Atomic_fetch_int_nops (Obj : Fir.Env_types.S) (Arg : Fir.Env_types.S) :
  C4f_utils.My_quickcheck.S_with_sexp
    with type t = Fir.Expression.t Fir.Atomic_fetch.t = struct
  (* We can't easily factor this into [expr_int], as we need the constant
     generator. *)
  type t = Fir.Expression.t Fir.Atomic_fetch.t [@@deriving sexp]

  (* TODO(@MattWindsor91): don't repeat self!*)

  let quickcheck_observer =
    [%quickcheck.observer: Fir.Expression.t Fir.Atomic_fetch.t]

  (* TODO(@MattWindsor91): expression shrinker *)
  let quickcheck_shrinker =
    [%quickcheck.shrinker: [%custom Q.Shrinker.atomic] Fir.Atomic_fetch.t]

  let quickcheck_generator =
    Atomic_fetch.gen_int_idem Obj.env ~const:(fun k -> const k Arg.env)
end

module Atomic_fetch_int_values
    (Obj : Fir.Env_types.S)
    (Arg : Fir.Env_types.S) :
  C4f_utils.My_quickcheck.S_with_sexp
    with type t = Fir.Expression.t Fir.Atomic_fetch.t =
  Atomic_fetch.Int (Obj) (Fir.Op.Fetch) (Int_values (Arg))

module Bool_values (E : Fir.Env_types.S) : sig
  type t = Fir.Expression.t [@@deriving sexp_of, quickcheck]
end = struct
  type t = Fir.Expression.t [@@deriving sexp]

  let quickcheck_generator : t Q.Generator.t = bool E.env

  let quickcheck_observer : t Q.Observer.t =
    [%quickcheck.observer: Fir.Expression.t]

  (* TODO(@MattWindsor91): implement this *)
  let quickcheck_shrinker : t Q.Shrinker.t = Q.Shrinker.atomic
end

let generate_known_bool_direct (target : bool) (var_ref : Fir.Expression.t)
    (value : bool) : Fir.Expression.t Q.Generator.t =
  Q.Generator.return
    ( if Bool.equal value target then var_ref
    else Fir.Expression.l_not var_ref )

(** [gen_known_bop_rel target operands] generates binary operations over
    [operands] that are relational and result in [target]. *)
let gen_known_bop_rel (target : bool) : Op.bop_gen option =
  Op.bop_with_output
    ~ops:(List.map ~f:(fun x -> Fir.Op.Binary.Rel x) Fir.Op.Binary.Rel.all)
    (Const (Fir.Constant.bool target))

(** [gen_known_bop_logic target operands] generates binary operations over
    [operands] that are logical (and, so, require boolean inputs) and result
    in [target]. *)
let gen_known_bop_logic (target : bool) : Op.bop_gen option =
  Op.bop_with_output
    ~ops:
      (List.map
         ~f:(fun x -> Fir.Op.Binary.Logical x)
         Fir.Op.Binary.Logical.all )
    (Const (Fir.Constant.bool target))

module Known_value_comparisons (Basic : sig
  module E : Fir.Env_types.S

  val target : bool
  (** The value to which the comparison should equal. *)
end) : sig
  type t = Fir.Expression.t [@@deriving sexp_of, quickcheck]
end = struct
  type t = Fir.Expression.t [@@deriving sexp_of]

  let generate_int (var_ref : Fir.Expression.t) (value : int) :
      t Q.Generator.t =
    (* TODO(@MattWindsor91): do more here? *)
    Option.value_exn
      (gen_known_bop_rel Basic.target)
      Op.basic_lift_k
      (Two (var_ref, Fir.Expression.int_lit value))

  let generate_bool (var_ref : Fir.Expression.t) (value : bool) :
      t Q.Generator.t =
    Q.Generator.union
      [ generate_known_bool_direct Basic.target var_ref value
      ; Option.value_exn
          (gen_known_bop_logic Basic.target)
          Op.basic_lift_k
          (Two (var_ref, Fir.Expression.bool_lit value))
      ; Option.value_exn
          (gen_known_bop_rel Basic.target)
          Op.basic_lift_k
          (Two (var_ref, Fir.Expression.bool_lit value)) ]

  let make_var_ref (id : C4f_common.C_id.t) (ty : Fir.Type.t) :
      Fir.Expression.t =
    (* TODO(@MattWindsor91): can we do more than SC here? *)
    let tid = C4f_common.C_named.make ty ~name:id in
    let lv = Fir.Lvalue.on_value_of_typed_id tid in
    if Fir.Type.is_atomic ty then
      Fir.Expression.atomic_load
        (Fir.Atomic_load.make
           ~src:(Fir.Address.ref_lvalue lv)
           ~mo:Fir.Mem_order.Seq_cst )
    else Fir.Expression.lvalue lv

  let quickcheck_generator : t Q.Generator.t =
    (* CAUTION: this generator is only defined when at least one known value
       exists. *)
    Q.Generator.Let_syntax.(
      let%bind id, (ty, value) =
        Basic.E.env |> Fir.Env.variables_with_known_values |> Map.to_alist
        |> Q.Generator.of_list
      in
      let var_ref : Fir.Expression.t = make_var_ref id ty in
      Fir.Constant.reduce value ~int:(generate_int var_ref)
        ~bool:(generate_bool var_ref))

  let quickcheck_observer : t Q.Observer.t =
    [%quickcheck.observer: Fir.Expression.t]

  (* TODO(@MattWindsor91): implement this *)
  let quickcheck_shrinker : t Q.Shrinker.t = Q.Shrinker.atomic
end

module Bool_known (E : Fir.Env_types.S) = struct
  module BV = Bool_values (E)

  module Tautologies : S = struct
    (* Define by using the bool-values quickcheck instance, but swap out the
       generator. *)
    include BV

    let quickcheck_generator : t Q.Generator.t = tautology E.env
  end

  module Falsehoods : S = struct
    include BV

    let quickcheck_generator : t Q.Generator.t = falsehood E.env
  end
end
