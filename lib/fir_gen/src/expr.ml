(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
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

let gen_atomic_fetch_k_nop
    (module Op_gen : Utils.My_quickcheck.S_with_sexp
      with type t = Fir.Op.Fetch.t) (k : Fir.Constant.t) (env : env)
    ~(gen_k : Fir.Constant.t -> env -> t Q.Generator.t) :
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

        let quickcheck_generator = gen_k k env
      end)
  in
  Expr_util.lift_loadlike [%quickcheck.generator: F.t]
    ~to_expr:Fir.Expression.atomic_fetch
    ~to_var:(Accessor.get Fir.Atomic_fetch.variable_of)
    ~env

(** [gen_atomic_fetch_zero_nop env ~gen_k] generates atomic fetches that use
    zero (from [gen_k]) to produce idempotent results. *)
let gen_atomic_fetch_zero_nop =
  gen_atomic_fetch_k_nop (module Fir.Op.Fetch.Gen_idem_zero_rhs) (Int 0)

(** [gen_atomic_fetch_neg1_nop env ~gen_k] generates atomic fetches that use
    negative-1 (from [gen_k]) to produce idempotent results. *)
let gen_atomic_fetch_neg1_nop =
  gen_atomic_fetch_k_nop
    ( module struct
      include Fir.Op.Fetch

      let quickcheck_generator = Q.Generator.return And
    end )
    (Int (-1))

let gen_atomic_fetch_nop (env : env)
    ~(gen_k : Fir.Constant.t -> env -> t Q.Generator.t) :
    (t * Fir.Env.Record.t) Q.Generator.t =
  (* TODO(@MattWindsor91): also add reflexivity here, but this'll require us
     to restrict to KV variables, and this'll need a guard. *)
  Q.Generator.weighted_union
    [ (4.0, gen_atomic_fetch_zero_nop env ~gen_k)
    ; (1.0, gen_atomic_fetch_neg1_nop env ~gen_k) ]

let gen_int_loadlike (env : env)
    ~(gen_k : Fir.Constant.t -> env -> t Q.Generator.t) :
    (t * Fir.Env.Record.t) Q.Generator.t =
  Q.Generator.weighted_union
    (Utils.My_list.eval_guards
       [ (true, fun () -> (3.0, Expr_prim.Int.gen_load env))
       ; ( Expr_util.has_ints env ~is_atomic:true
         , fun () -> (1.0, gen_atomic_fetch_nop env ~gen_k) ) ])

module Int_value_gen = struct
  (** Generates the terminal integer expressions. *)
  let base_generators (env : env)
      ~(gen_k : Fir.Constant.t -> env -> t Q.Generator.t) :
      (float * t Q.Generator.t) list =
    Utils.My_list.eval_guards
      [ (true, fun () -> (3.0, gen_k (Int 0) env))
      ; (true, fun () -> (3.0, Expr_prim.Int.gen env))
        (* Prim.Int.gen subsumes Prim.Int.gen_load, so this serves to cover
           the remaining case(s) in int_loadlike. *)
      ; ( Expr_util.has_ints env ~is_atomic:true
        , fun () ->
            (2.0, Q.Generator.map ~f:fst (gen_atomic_fetch_nop env ~gen_k))
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

  let gen (env : env) ~(gen_k : Fir.Constant.t -> env -> t Q.Generator.t) :
      t Q.Generator.t =
    Q.Generator.weighted_recursive_union
      (base_generators env ~gen_k)
      ~f:recursive_generators
end

let rec gen_k (k : Fir.Constant.t) (env : env) : t Q.Generator.t =
  let gen_load env = gen_int_loadlike env ~gen_k in
  Expr_const.gen k env ~gen_load ~gen_arb:gen_int

and gen_int (env : env) : t Q.Generator.t = Int_value_gen.gen env ~gen_k

module Int_zeroes (E : Fir.Env_types.S) = struct
  type t = Fir.Expression.t [@@deriving sexp]

  let quickcheck_generator : t Q.Generator.t = gen_k (Int 0) E.env

  let quickcheck_observer : t Q.Observer.t =
    [%quickcheck.observer: Fir.Expression.t]

  (* TODO(@MattWindsor91): implement this *)
  let quickcheck_shrinker : t Q.Shrinker.t = Q.Shrinker.atomic
end

module Int_values (E : Fir.Env_types.S) = struct
  type t = Fir.Expression.t [@@deriving sexp]

  let quickcheck_generator : t Q.Generator.t = gen_int E.env

  let quickcheck_observer : t Q.Observer.t =
    [%quickcheck.observer: Fir.Expression.t]

  (* TODO(@MattWindsor91): implement this *)
  let quickcheck_shrinker : t Q.Shrinker.t = Q.Shrinker.atomic
end

module Atomic_fetch_int_zero_nops
    (Obj : Fir.Env_types.S)
    (Arg : Fir.Env_types.S) :
  Act_utils.My_quickcheck.S_with_sexp
    with type t = Fir.Expression.t Fir.Atomic_fetch.t =
  Atomic_fetch.Int (Obj) (Fir.Op.Fetch.Gen_idem_zero_rhs) (Int_zeroes (Arg))

module Atomic_fetch_int_refl_nops (Obj : Fir.Env_types.S) :
  Act_utils.My_quickcheck.S_with_sexp
    with type t = Fir.Expression.t Fir.Atomic_fetch.t = struct
  type t = Fir.Expression.t Fir.Atomic_fetch.t [@@deriving sexp]

  (* TODO(@MattWindsor91): a LOT of this overlaps with the atomic fetch
     generator... *)

  module A = Address.Atomic_int_pointers (Obj)
  module Z = Int_zeroes (Obj)

  let quickcheck_observer =
    [%quickcheck.observer: Fir.Expression.t Fir.Atomic_fetch.t]

  (* TODO(@MattWindsor91): expression shrinker *)
  let quickcheck_shrinker =
    [%quickcheck.shrinker: [%custom Q.Shrinker.atomic] Fir.Atomic_fetch.t]

  let gen_address : (Fir.Address.t * Fir.Env.Record.t) Q.Generator.t =
    Expr_util.with_record A.quickcheck_generator
      ~to_var:(Accessor.get Fir.Address.variable_of)
      ~env:Obj.env

  let gen_op (obj : Fir.Address.t) (arg : Fir.Expression.t) :
      Fir.Expression.t Fir.Atomic_fetch.t Q.Generator.t =
    Q.Generator.Let_syntax.(
      let%bind op = Fir.Op.Fetch.Gen_idem_refl.quickcheck_generator in
      (* 'all values are permitted':
         https://en.cppreference.com/w/c/atomic/atomic_fetch_add etc. *)
      let%map mo = Fir.Mem_order.quickcheck_generator in
      Fir.Atomic_fetch.make ~obj ~arg ~mo ~op)

  let quickcheck_generator =
    Expr_util.gen_kv_refl ~gen_load:gen_address ~gen_op
end

module Atomic_fetch_int_nops (Obj : Fir.Env_types.S) (Arg : Fir.Env_types.S) :
  Act_utils.My_quickcheck.S_with_sexp
    with type t = Fir.Expression.t Fir.Atomic_fetch.t = struct
  type t = Fir.Expression.t Fir.Atomic_fetch.t [@@deriving sexp]

  (* TODO(@MattWindsor91): don't repeat self!*)

  let quickcheck_observer =
    [%quickcheck.observer: Fir.Expression.t Fir.Atomic_fetch.t]

  (* TODO(@MattWindsor91): expression shrinker *)
  let quickcheck_shrinker =
    [%quickcheck.shrinker: [%custom Q.Shrinker.atomic] Fir.Atomic_fetch.t]

  module Z = Atomic_fetch_int_zero_nops (Obj) (Arg)
  module R = Atomic_fetch_int_refl_nops (Obj)

  let quickcheck_generator =
    Q.Generator.union [Z.quickcheck_generator; R.quickcheck_generator]
end

module Atomic_fetch_int_values
    (Obj : Fir.Env_types.S)
    (Arg : Fir.Env_types.S) :
  Act_utils.My_quickcheck.S_with_sexp
    with type t = Fir.Expression.t Fir.Atomic_fetch.t =
  Atomic_fetch.Int (Obj) (Fir.Op.Fetch) (Int_values (Arg))

module Bool_values (E : Fir.Env_types.S) : sig
  type t = Fir.Expression.t [@@deriving sexp_of, quickcheck]
end = struct
  type t = Fir.Expression.t [@@deriving sexp]

  module Iv = Int_values (E)

  (* As with Int_values. TODO(@MattWindsor91): extract this pattern somehow? *)

  let gen_int_relational : t Q.Generator.t =
    Q.Generator.Let_syntax.(
      let%map l = [%quickcheck.generator: Iv.t]
      and r = [%quickcheck.generator: Iv.t]
      and op = [%quickcheck.generator: Fir.Op.Binary.Rel.t] in
      Fir.Expression.bop (Fir.Op.Binary.Rel op) l r)

  (** Generates the terminal Boolean expressions. *)
  let base_generators : (float * t Q.Generator.t) list =
    (* Use thunks here to prevent accidentally evaluating a generator that
       can't possibly work---eg, an atomic load when we don't have any atomic
       variables. *)
    Utils.My_list.eval_guards
      [ (true, fun () -> (5.0, Expr_prim.Bool.gen E.env))
      ; (true, fun () -> (5.0, gen_int_relational)) ]

  let recursive_generators (mu : t Q.Generator.t) :
      (float * t Q.Generator.t) list =
    [ (3.0, Q.Generator.map2 mu mu ~f:Fir.Expression.l_and)
    ; (3.0, Q.Generator.map2 mu mu ~f:Fir.Expression.l_or)
    ; (2.0, Q.Generator.map mu ~f:Fir.Expression.l_not) ]

  let quickcheck_generator : t Q.Generator.t =
    Q.Generator.weighted_recursive_union base_generators
      ~f:recursive_generators

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
let gen_known_bop_rel (target : bool) :
    (Op.Operand_set.t -> Fir.Expression.t Q.Generator.t) option =
  Op.bop
    (module Fir.Op.Binary.Rel)
    ~promote:(fun x -> Fir.Op.Binary.Rel x)
    ~out:(Const (Fir.Constant.bool target))

(** [gen_known_bop_logic target operands] generates binary operations over
    [operands] that are logical (and, so, require boolean inputs) and result
    in [target]. *)
let gen_known_bop_logic (target : bool) :
    (Op.Operand_set.t -> Fir.Expression.t Q.Generator.t) option =
  Op.bop
    (module Fir.Op.Binary.Logical)
    ~promote:(fun x -> Fir.Op.Binary.Logical x)
    ~out:(Const (Fir.Constant.bool target))

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
      (Two (var_ref, Fir.Expression.int_lit value))

  let generate_bool (var_ref : Fir.Expression.t) (value : bool) :
      t Q.Generator.t =
    Q.Generator.union
      [ generate_known_bool_direct Basic.target var_ref value
      ; Option.value_exn
          (gen_known_bop_logic Basic.target)
          (Two (var_ref, Fir.Expression.bool_lit value))
      ; Option.value_exn
          (gen_known_bop_rel Basic.target)
          (Two (var_ref, Fir.Expression.bool_lit value)) ]

  let make_var_ref (id : Act_common.C_id.t) (ty : Fir.Type.t) :
      Fir.Expression.t =
    (* TODO(@MattWindsor91): can we do more than SC here? *)
    let tid = Act_common.C_named.make ty ~name:id in
    let lv = Fir.Lvalue.on_value_of_typed_id tid in
    if Fir.Type.is_atomic ty then
      Fir.Expression.atomic_load
        (Fir.Atomic_load.make
           ~src:(Fir.Address.ref_lvalue lv)
           ~mo:Fir.Mem_order.Seq_cst)
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

  type t = BV.t

  (** [gen_bop_both bop mu] is a generator that creates binary operations
      over [bop] where both sides are known-value Booleans created using
      [mu].

      The [bop] should be an operator where giving the intended known truth
      value on both sides yields that truth value. For tautologies, [bop]
      will usually be [l_and]; for falsehoods, [l_or]. *)
  let gen_bop_both (bop : Fir.Op.Binary.t) (mu : t Q.Generator.t) :
      t Q.Generator.t =
    Q.Generator.map2 mu mu ~f:(Fir.Expression.bop bop)

  (** [gen_bop_short bop mu] is a generator that creates binary operations
      over [bop] where the LHS short-circuits using a known-value Boolean
      created using [mu], and the RHS uses the generic value generator.

      The [bop] should be an operator where giving the intended known truth
      value on at least one side yields that truth value. For tautologies,
      [bop] will usually be [l_or]; for falsehoods, [l_and]. *)
  let gen_bop_short (bop : Fir.Op.Binary.t) (mu : t Q.Generator.t) :
      t Q.Generator.t =
    (* Ensuring short-circuit by using tautological generator first. *)
    Q.Generator.map2 mu BV.quickcheck_generator ~f:(Fir.Expression.bop bop)

  (** [gen_bop_long bop mu] is a generator that creates binary operations
      over [bop] where the LHS uses a generic value generator, but the RHS
      uses the known-value generator [mu].

      The [bop] should be an operator where giving the intended known truth
      value on at least one side yields that truth value. For tautologies,
      [bop] will usually be [l_or]; for falsehoods, [l_and]. *)
  let gen_bop_long (bop : Fir.Op.Binary.t) (mu : t Q.Generator.t) :
      t Q.Generator.t =
    (* We need at least one of the terms to be tautological; this is the
       'long' version that only guarantees the RHS is. *)
    Q.Generator.map2 BV.quickcheck_generator mu ~f:(Fir.Expression.bop bop)

  let gen_not (mu : t Q.Generator.t) : t Q.Generator.t =
    Q.Generator.map mu ~f:Fir.Expression.l_not

  let is_var_eq_tenable : bool Lazy.t =
    lazy (not (Map.is_empty (Fir.Env.variables_with_known_values E.env)))

  (* Tautology and falsehood generators are mutually recursive at the
     recursive-generator level. It's easier to do this mutual recursion over
     functions than modules, but we expose a modular interface in the end. *)

  let base_generators (target : bool) : t Q.Generator.t list =
    (* Use thunks here to prevent accidentally evaluating a generator that
       can't possibly work---eg, an atomic load when we don't have any atomic
       variables. *)
    Utils.My_list.eval_guards
      [ (true, fun () -> Q.Generator.return (Fir.Expression.bool_lit target))
      ; ( Lazy.force is_var_eq_tenable
        , fun () ->
            let module Var_eq = Known_value_comparisons (struct
              module E = E

              let target = target
            end) in
            Var_eq.quickcheck_generator ) ]

  let mk_recursive_generator (mu : t Q.Generator.t)
      ~(both_bop : Fir.Op.Binary.t) ~(circuiting_bop : Fir.Op.Binary.t)
      ~(negated_gen : t Q.Generator.t Lazy.t) : t Q.Generator.t list =
    Utils.My_list.eval_guards
      [ (true, fun () -> gen_not (Q.Generator.of_lazy negated_gen))
      ; (true, fun () -> gen_bop_both both_bop mu)
      ; (true, fun () -> gen_bop_short circuiting_bop mu)
      ; (true, fun () -> gen_bop_long circuiting_bop mu) ]

  let tt_recursive_generators (mu : t Q.Generator.t)
      ~(negated_gen : t Q.Generator.t Lazy.t) : t Q.Generator.t list =
    mk_recursive_generator mu ~both_bop:Fir.Op.Binary.l_and
      ~circuiting_bop:Fir.Op.Binary.l_or ~negated_gen

  let ff_recursive_generators (mu : t Q.Generator.t)
      ~(negated_gen : t Q.Generator.t Lazy.t) : t Q.Generator.t list =
    mk_recursive_generator mu ~both_bop:Fir.Op.Binary.l_or
      ~circuiting_bop:Fir.Op.Binary.l_and ~negated_gen

  let rec tt_generator : t Q.Generator.t Lazy.t =
    lazy
      (Q.Generator.recursive_union (base_generators true)
         ~f:(tt_recursive_generators ~negated_gen:ff_generator))

  and ff_generator : t Q.Generator.t Lazy.t =
    lazy
      (Q.Generator.recursive_union (base_generators false)
         ~f:(ff_recursive_generators ~negated_gen:tt_generator))

  module Tautologies : S = struct
    (* Define by using the bool-values quickcheck instance, but swap out the
       generator. *)
    include BV

    let quickcheck_generator : t Q.Generator.t = Lazy.force tt_generator
  end

  module Falsehoods : S = struct
    include BV

    let quickcheck_generator : t Q.Generator.t = Lazy.force ff_generator
  end
end

let bool (env : env) : Fir.Expression.t Q.Generator.t =
  let module G = Bool_values (struct
    let env = env
  end) in
  G.quickcheck_generator

let tautology (env : env) : Fir.Expression.t Q.Generator.t =
  let module G = Bool_known (struct
    let env = env
  end) in
  G.Tautologies.quickcheck_generator

let falsehood (env : env) : Fir.Expression.t Q.Generator.t =
  let module G = Bool_known (struct
    let env = env
  end) in
  G.Falsehoods.quickcheck_generator
