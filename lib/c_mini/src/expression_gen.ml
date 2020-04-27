(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Q = Base_quickcheck

module type S =
  Act_utils.My_quickcheck.S_with_sexp with type t = Expression.t

let eval_guards : (bool * (unit -> 'a)) list -> 'a list =
  List.filter_map ~f:(fun (g, f) -> if g then Some (f ()) else None)

let gen_int32 : Expression.t Q.Generator.t =
  Q.Generator.map ~f:Expression.constant Constant.gen_int32

(** Primitive integer expressions over a given environment. *)
module Int_prims (E : Env_types.S) = struct
  type t = Expression.t [@@deriving sexp]

  let lift (g : 'a Q.Generator.t) ~(to_expr : 'a -> t)
      ~(to_var : 'a -> Act_common.C_id.t) : (t * Env.Record.t) Q.Generator.t
      =
    Q.Generator.filter_map g ~f:(fun x ->
        Option.(x |> to_var |> Map.find E.env >>| fun r -> (to_expr x, r)))

  (* These 'let module's can't be lifted outside of their thunks, because
     otherwise they'll evaluate at instantiation of this functor, and not all
     of them will be available in all environments. *)

  let gen_atomic_load () : (t * Env.Record.t) Q.Generator.t =
    let module AL = Atomic_load.Quickcheck_atomic_ints (E) in
    lift [%quickcheck.generator: AL.t] ~to_expr:Expression.atomic_load
      ~to_var:Atomic_load.variable_of

  let gen_atomic_fetch_nop ~(gen_zero : t Q.Generator.t) :
      (t * Env.Record.t) Q.Generator.t =
    let module F =
      Atomic_fetch.Quickcheck_generic
        (Address_gen.Atomic_int_pointers (E)) (Op.Fetch)
        (struct
          type nonrec t = t

          let sexp_of_t = sexp_of_t

          let quickcheck_observer = Expression.quickcheck_observer

          let quickcheck_shrinker = Q.Shrinker.atomic

          let quickcheck_generator = gen_zero
        end)
    in
    lift [%quickcheck.generator: F.t] ~to_expr:Expression.atomic_fetch
      ~to_var:Atomic_fetch.variable_of

  let gen_lvalue () : (t * Env.Record.t) Q.Generator.t =
    let module LV = Lvalue_gen.Int_values (E) in
    lift [%quickcheck.generator: LV.t] ~to_expr:Expression.lvalue
      ~to_var:Lvalue.variable_of

  let has_ints ~(atomic : bool) : bool =
    Env.has_variables_of_basic_type E.env ~basic:Type.Basic.(int ~atomic ())

  let gen_load_with_record ~(gen_zero : t Q.Generator.t) :
      (t * Env.Record.t) Q.Generator.t list =
    eval_guards
      [ (has_ints ~atomic:true, gen_atomic_load)
      ; (has_ints ~atomic:true, fun () -> gen_atomic_fetch_nop ~gen_zero)
      ; (has_ints ~atomic:false, gen_lvalue) ]

  let gen_load ~(gen_zero : t Q.Generator.t) : t Q.Generator.t list =
    List.map ~f:(Q.Generator.map ~f:fst) (gen_load_with_record ~gen_zero)

  let gen_prim ~(gen_zero : t Q.Generator.t) : t Q.Generator.t =
    Q.Generator.union ([gen_int32] @ gen_load ~gen_zero)
end

module Int_zeroes (E : Env_types.S) = struct
  type t = Expression.t [@@deriving sexp]

  module Det_env = struct
    let env = Env.filter_to_known_values E.env
  end

  (** Generates primitive expressions that, if appropriately tagged with
      dependencies, are statically known to hold the same value, and so can
      be used as a zero source through expressions of the form [x - x]. *)
  module Det_prims = Int_prims (Det_env)

  (** Generates terminal zero integer expressions. *)
  let base_generators : t Q.Generator.t list =
    [Q.Generator.return (Expression.int_lit 0)]

  let gen_cancel ~(mu : t Q.Generator.t) : t Q.Generator.t =
    Q.Generator.Let_syntax.(
      let%map p = Det_prims.gen_prim ~gen_zero:mu in
      Expression.Infix.(p - p))

  let gen_kv_sub ~(mu : t Q.Generator.t) : t Q.Generator.t =
    Q.Generator.filter_map
      (Q.Generator.union (Det_prims.gen_load_with_record ~gen_zero:mu))
      ~f:(fun (l, r) ->
        Option.map (Env.Record.known_value r) ~f:(fun kv ->
            Expression.(Infix.(l - constant kv))))

  let recursive_generators (mu : t Q.Generator.t) : t Q.Generator.t list =
    eval_guards
      [ (true, fun () -> gen_cancel ~mu)
      ; ( Det_prims.has_ints ~atomic:true || Det_prims.has_ints ~atomic:false
        , fun () -> gen_kv_sub ~mu ) ]

  let quickcheck_generator : t Q.Generator.t =
    Q.Generator.recursive_union base_generators ~f:recursive_generators

  let quickcheck_observer : t Q.Observer.t =
    [%quickcheck.observer: Expression.t]

  (* TODO(@MattWindsor91): implement this *)
  let quickcheck_shrinker : t Q.Shrinker.t = Q.Shrinker.atomic
end

module Int_values (E : Env_types.S) = struct
  type t = Expression.t [@@deriving sexp]

  module Z = Int_zeroes (E)
  module P = Int_prims (E)

  (** Generates the terminal integer expressions. *)
  let base_generators : t Q.Generator.t list =
    [Z.quickcheck_generator; P.gen_prim ~gen_zero:Z.quickcheck_generator]

  let quickcheck_generator : t Q.Generator.t =
    (* TODO(@MattWindsor91): find some 'safe' recursive ops. *)
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
    Q.Generator.map2 [%quickcheck.generator: Iv.t]
      [%quickcheck.generator: Iv.t] ~f:Expression.eq

  let gen_const : t Q.Generator.t =
    Q.Generator.map ~f:Expression.bool_lit Q.([%quickcheck.generator: bool])

  (** Generates the terminal Boolean expressions. *)
  let base_generators : (float * t Q.Generator.t) list =
    (* Use thunks here to prevent accidentally evaluating a generator that
       can't possibly work---eg, an atomic load when we don't have any atomic
       variables. *)
    [(1.0, gen_const); (4.0, gen_int_relational)]
    @ eval_guards
        [ ( Env.has_variables_of_basic_type E.env ~basic:Type.Basic.(bool ())
          , fun () -> (4.0, gen_bool_lvalue ()) ) ]

  let recursive_generators (mu : t Q.Generator.t) :
      (float * t Q.Generator.t) list =
    [ (3.0, Q.Generator.map2 mu mu ~f:Expression.l_and)
    ; (3.0, Q.Generator.map2 mu mu ~f:Expression.l_or)
    ; (2.0, Q.Generator.map mu ~f:Expression.l_not) ]

  let quickcheck_generator : t Q.Generator.t =
    Q.Generator.weighted_recursive_union base_generators
      ~f:recursive_generators

  let quickcheck_observer : t Q.Observer.t =
    [%quickcheck.observer: Expression.t]

  (* TODO(@MattWindsor91): implement this *)
  let quickcheck_shrinker : t Q.Shrinker.t = Q.Shrinker.atomic
end

module Known_value_comparisons (E : Env_types.S) : sig
  type t = Expression.t [@@deriving sexp_of, quickcheck]
end = struct
  type t = Expression.t [@@deriving sexp_of]

  let generate_int (var_ref : Expression.t) (value : int) : t Q.Generator.t =
    (* TODO(@MattWindsor91): do more here? *)
    Q.Generator.Let_syntax.(
      let%map op =
        Q.Generator.of_list Op.Binary.[Eq]
        (* TODO(@MattWindsor91): inequalities *)
      in
      Expression.(bop op var_ref (int_lit value)))

  let generate_bool (var_ref : Expression.t) (value : bool) : t Q.Generator.t
      =
    (* TODO(@MattWindsor91): do more here? *)
    let basic_eq = Expression.(eq var_ref (bool_lit value)) in
    let candidates =
      if value then [var_ref (* implicit true *); basic_eq] else [basic_eq]
    in
    Q.Generator.of_list candidates

  let make_var_ref (id : Act_common.C_id.t) (ty : Type.t) : Expression.t =
    (* TODO(@MattWindsor91): can we do more than SC here? *)
    let tid = Act_common.C_named.make ty ~name:id in
    let lv = Lvalue.on_value_of_typed_id tid in
    if Type.is_atomic ty then
      Expression.atomic_load
        (Atomic_load.make ~src:(Address.ref_lvalue lv) ~mo:Mem_order.Seq_cst)
    else Expression.lvalue lv

  let quickcheck_generator : t Q.Generator.t =
    (* CAUTION: this generator is only defined when at least one known value
       exists. *)
    Q.Generator.Let_syntax.(
      let%bind id, (ty, value) =
        E.env |> Env.variables_with_known_values |> Map.to_alist
        |> Q.Generator.of_list
      in
      let var_ref : Expression.t = make_var_ref id ty in
      Constant.reduce value ~int:(generate_int var_ref)
        ~bool:(generate_bool var_ref))

  let quickcheck_observer : t Q.Observer.t =
    [%quickcheck.observer: Expression.t]

  (* TODO(@MattWindsor91): implement this *)
  let quickcheck_shrinker : t Q.Shrinker.t = Q.Shrinker.atomic
end

module Bool_known (E : Env_types.S) = struct
  module BV = Bool_values (E)

  type t = BV.t

  (** [gen_bop_both bop mu] is a generator that creates binary operations
      over [bop] where both sides are known-value Booleans created using
      [mu].

      The [bop] should be an operator where giving the intended known truth
      value on both sides yields that truth value. For tautologies, [bop]
      will usually be [l_and]; for falsehoods, [l_or]. *)
  let gen_bop_both (bop : Op.Binary.t) (mu : t Q.Generator.t) :
      t Q.Generator.t =
    Q.Generator.map2 mu mu ~f:(Expression.bop bop)

  (** [gen_bop_short bop mu] is a generator that creates binary operations
      over [bop] where the LHS short-circuits using a known-value Boolean
      created using [mu], and the RHS uses the generic value generator.

      The [bop] should be an operator where giving the intended known truth
      value on at least one side yields that truth value. For tautologies,
      [bop] will usually be [l_or]; for falsehoods, [l_and]. *)
  let gen_bop_short (bop : Op.Binary.t) (mu : t Q.Generator.t) :
      t Q.Generator.t =
    (* Ensuring short-circuit by using tautological generator first. *)
    Q.Generator.map2 mu BV.quickcheck_generator ~f:(Expression.bop bop)

  (** [gen_bop_long bop mu] is a generator that creates binary operations
      over [bop] where the LHS uses a generic value generator, but the RHS
      uses the known-value generator [mu].

      The [bop] should be an operator where giving the intended known truth
      value on at least one side yields that truth value. For tautologies,
      [bop] will usually be [l_or]; for falsehoods, [l_and]. *)
  let gen_bop_long (bop : Op.Binary.t) (mu : t Q.Generator.t) :
      t Q.Generator.t =
    (* We need at least one of the terms to be tautological; this is the
       'long' version that only guarantees the RHS is. *)
    Q.Generator.map2 BV.quickcheck_generator mu ~f:(Expression.bop bop)

  let gen_not (mu : t Q.Generator.t) : t Q.Generator.t =
    Q.Generator.map mu ~f:Expression.l_not

  let is_var_eq_tenable : bool Lazy.t =
    lazy (not (Map.is_empty (Env.variables_with_known_values E.env)))

  (* Tautology and falsehood generators are mutually recursive at the
     recursive-generator level. It's easier to do this mutual recursion over
     functions than modules, but we expose a modular interface in the end. *)

  let tt_base_generators : t Q.Generator.t list =
    (* Use thunks here to prevent accidentally evaluating a generator that
       can't possibly work---eg, an atomic load when we don't have any atomic
       variables. *)
    eval_guards
      [ (true, fun () -> Q.Generator.return Expression.truth)
      ; ( Lazy.force is_var_eq_tenable
        , fun () ->
            let module Var_eq = Known_value_comparisons (E) in
            Var_eq.quickcheck_generator ) ]

  let ff_base_generators : t Q.Generator.t list =
    (* TODO(@MattWindsor91): known-value false comparisons *)
    eval_guards [(true, fun () -> Q.Generator.return Expression.falsehood)]

  let mk_recursive_generator (mu : t Q.Generator.t) ~(both_bop : Op.Binary.t)
      ~(circuiting_bop : Op.Binary.t) ~(negated_gen : t Q.Generator.t Lazy.t)
      : t Q.Generator.t list =
    eval_guards
      [ (true, fun () -> gen_not (Q.Generator.of_lazy negated_gen))
      ; (true, fun () -> gen_bop_both both_bop mu)
      ; (true, fun () -> gen_bop_short circuiting_bop mu)
      ; (true, fun () -> gen_bop_long circuiting_bop mu) ]

  let tt_recursive_generators (mu : t Q.Generator.t)
      ~(negated_gen : t Q.Generator.t Lazy.t) : t Q.Generator.t list =
    mk_recursive_generator mu ~both_bop:Op.Binary.l_and
      ~circuiting_bop:Op.Binary.l_or ~negated_gen

  let ff_recursive_generators (mu : t Q.Generator.t)
      ~(negated_gen : t Q.Generator.t Lazy.t) : t Q.Generator.t list =
    mk_recursive_generator mu ~both_bop:Op.Binary.l_or
      ~circuiting_bop:Op.Binary.l_and ~negated_gen

  let rec tt_generator : t Q.Generator.t Lazy.t =
    lazy
      (Q.Generator.recursive_union tt_base_generators
         ~f:(tt_recursive_generators ~negated_gen:ff_generator))

  and ff_generator : t Q.Generator.t Lazy.t =
    lazy
      (Q.Generator.recursive_union ff_base_generators
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

let gen_bools (env : Env.t) : Expression.t Q.Generator.t =
  let module G = Bool_values (struct
    let env = env
  end) in
  G.quickcheck_generator

let gen_tautologies (env : Env.t) : Expression.t Q.Generator.t =
  let module G = Bool_known (struct
    let env = env
  end) in
  G.Tautologies.quickcheck_generator

let gen_falsehoods (env : Env.t) : Expression.t Q.Generator.t =
  let module G = Bool_known (struct
    let env = env
  end) in
  G.Falsehoods.quickcheck_generator
