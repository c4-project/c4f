(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Core_kernel
module Ac = Act_common
module Identifier = Act_c_lang.Ast_basic.Identifier

module Bop = struct
  type t = Eq | L_and | L_or
  [@@deriving sexp, variants, compare, equal, quickcheck]
end

type t =
  | Constant of Constant.t
  | Lvalue of Lvalue.t
  | Atomic_load of Atomic_load.t
  | Bop of Bop.t * t * t
[@@deriving sexp, variants, compare, equal]

let int_lit (i : int) : t = constant (Constant.int i)

let bool_lit (b : bool) : t = constant (Constant.bool b)

let eq : t -> t -> t = bop Bop.Eq

let l_and : t -> t -> t = bop Bop.L_and

let l_or : t -> t -> t = bop Bop.L_or

let map (expr : t) ~(constant : Constant.t -> 'a) ~(lvalue : Lvalue.t -> 'a)
    ~(atomic_load : Atomic_load.t -> 'a) ~(bop : Bop.t -> t -> t -> 'a) : 'a
    =
  match expr with
  | Constant k ->
      constant k
  | Lvalue l ->
      lvalue l
  | Atomic_load ld ->
      atomic_load ld
  | Bop (b, x, y) ->
      bop b x y

let reduce (expr : t) ~(constant : Constant.t -> 'a)
    ~(lvalue : Lvalue.t -> 'a) ~(atomic_load : Atomic_load.t -> 'a)
    ~(bop : Bop.t -> 'a -> 'a -> 'a) : 'a =
  let rec mu (expr : t) =
    map expr ~constant ~lvalue ~atomic_load ~bop:(fun b l r ->
        bop b (mu l) (mu r))
  in
  mu expr

let anonymise = function
  | Constant k ->
      `A k
  | Lvalue l ->
      `B l
  | Bop (b, x, y) ->
      `C (b, x, y)
  | Atomic_load ld ->
      `D ld

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Address

  module On_monad (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)
    module A = Atomic_load.On_addresses.On_monad (M)

    let rec map_m x ~f =
      Variants.map x
        ~constant:(F.proc_variant1 M.return)
        ~lvalue:(F.proc_variant1 M.return)
        ~bop:
          (F.proc_variant3 (fun (b, l, r) ->
               M.Let_syntax.(
                 let%bind l' = map_m l ~f in
                 let%map r' = map_m r ~f in
                 (b, l', r'))))
        ~atomic_load:(F.proc_variant1 (A.map_m ~f))
  end
end)

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Lvalue

  module On_monad (M : Monad.S) = struct
    module A = Atomic_load.On_lvalues.On_monad (M)
    module F = Travesty.Traversable.Helpers (M)

    let rec map_m x ~f =
      Variants.map x
        ~constant:(F.proc_variant1 M.return)
        ~lvalue:(F.proc_variant1 f)
        ~bop:
          (F.proc_variant3 (fun (b, l, r) ->
               M.Let_syntax.(
                 let%bind l' = map_m l ~f in
                 let%map r' = map_m r ~f in
                 (b, l', r'))))
        ~atomic_load:(F.proc_variant1 (A.map_m ~f))
  end
end)

module On_identifiers :
  Travesty.Traversable_types.S0
    with type t = t
     and type Elt.t = Identifier.t =
  Travesty.Traversable.Chain0 (On_lvalues) (Lvalue.On_identifiers)

module Type_check (E : Env_types.S) = struct
  module Lv = Lvalue.Type_check (E)
  module Ld = Atomic_load.Type_check (E)

  let type_of_resolved_bop (b : Bop.t) (l_type : Type.t) (r_type : Type.t) :
      Type.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind u_type =
        if Type.equal l_type r_type then Or_error.return l_type
        else
          Or_error.error_s
            [%message
              "Operand types are not equal"
                ~operator:(b : Bop.t)
                ~left:(l_type : Type.t)
                ~right:(r_type : Type.t)]
      in
      match b with
      | Eq ->
          return Type.(bool ())
      | L_and | L_or ->
          if Type.equal Type.(bool ()) u_type then Or_error.return u_type
          else
            Or_error.error_s
              [%message
                "Operand types must be 'bool'"
                  ~operator:(b : Bop.t)
                  ~left:(l_type : Type.t)
                  ~right:(r_type : Type.t)])

  let rec type_of : t -> Type.t Or_error.t = function
    | Constant k ->
        Or_error.return (Constant.type_of k)
    | Lvalue l ->
        Lv.type_of l
    | Bop (b, l, r) ->
        type_of_bop b l r
    | Atomic_load ld ->
        Ld.type_of ld

  and type_of_bop (b : Bop.t) (l : t) (r : t) : Type.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind l_type = type_of l and r_type = type_of r in
      type_of_resolved_bop b l_type r_type)
end

let quickcheck_observer : t Base_quickcheck.Observer.t =
  Quickcheck.Observer.(
    fixed_point (fun mu ->
        unmap ~f:anonymise
          [%quickcheck.observer:
            [ `A of Constant.t
            | `B of Lvalue.t
            | `C of Bop.t * [%custom mu] * [%custom mu]
            | `D of Atomic_load.t ]]))

module Eval = struct
  let eval_logical (l : t) (r : t) ~(short_value : bool)
      ~(mu : t -> Constant.t Or_error.t) : Constant.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind l_const = mu l in
      let%bind l_value = Constant.as_bool l_const in
      if Bool.equal l_value short_value then Or_error.return l_const
      else mu r)

  let eval_land = eval_logical ~short_value:false

  let eval_lor = eval_logical ~short_value:true

  let eval_eq (l : t) (r : t) ~(mu : t -> Constant.t Or_error.t) =
    Or_error.Let_syntax.(
      let%bind l_const = mu l in
      let%bind r_const = mu r in
      if
        Comparable.lift [%equal: Type.t] ~f:Constant.type_of l_const r_const
      then
        Or_error.return
          (Constant.bool ([%equal: Constant.t] l_const r_const))
      else
        Or_error.error_s
          [%message
            "eq: types of constants are incompatible"
              ~left:(l_const : Constant.t)
              ~right:(r_const : Constant.t)])

  let eval_atomic_load (atomic_load : Atomic_load.t)
      ~(env : Address.t -> Constant.t Or_error.t) : Constant.t Or_error.t =
    (* We don't specifically handle memory order here, since we assume that
       the known-values environment refers to things that are already fully
       propagated through memory. *)
    atomic_load |> Atomic_load.src |> Address.deref |> env

  let as_constant (expr : t) ~(env : Address.t -> Constant.t Or_error.t) :
      Constant.t Or_error.t =
    let rec mu : t -> Constant.t Or_error.t =
      (* We map rather than reduce to support short-circuiting evaluation. *)
      map ~constant:Or_error.return ~lvalue:(Fn.compose env Address.lvalue)
        ~atomic_load:(eval_atomic_load ~env) ~bop:(function
        | L_and ->
            eval_land ~mu
        | L_or ->
            eval_lor ~mu
        | Eq ->
            eval_eq ~mu)
    in
    mu expr

  let empty_env (_ : Address.t) : Constant.t Or_error.t =
    Or_error.error_string
      "tried to access an address in an empty environment"
end
