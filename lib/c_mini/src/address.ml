(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common

module M = struct
  type t = Lvalue of Lvalue.t | Ref of t
  [@@deriving sexp, variants, compare, equal]
end

include M
include Comparable.Make (M)

let of_variable (v : Ac.C_id.t) : t = Lvalue (Lvalue.variable v)

let of_variable_str_exn (vs : string) : t =
  lvalue (Lvalue.of_variable_str_exn vs)

let of_variable_ref (v : Ac.C_id.t) : t = Ref (of_variable v)

let ref_lvalue (l : Lvalue.t) : t =
  match Lvalue.un_deref l with
  | Ok l' ->
      lvalue l'
  | Error _ ->
      ref (lvalue l)

let ref_normal : t -> t = function Lvalue k -> ref_lvalue k | x -> ref x

let rec reduce (addr : t) ~(lvalue : Lvalue.t -> 'a) ~(ref : 'a -> 'a) : 'a =
  match addr with
  | Lvalue lv ->
      lvalue lv
  | Ref rest ->
      ref (reduce rest ~lvalue ~ref)

let lvalue_of : t -> Lvalue.t = reduce ~lvalue:Fn.id ~ref:Fn.id

let ref_depth = reduce ~lvalue:(Fn.const 0) ~ref:Int.succ

let normalise (addr : t) : t =
  Fn.apply_n_times ~n:(ref_depth addr) ref_normal (lvalue (lvalue_of addr))

let deref (addr : t) : t =
  match normalise addr with
  | Ref x ->
      x
  | Lvalue l ->
      Lvalue (Lvalue.deref l)

let as_lvalue (addr : t) : Lvalue.t Or_error.t =
  match normalise addr with
  | Lvalue x ->
      Or_error.return x
  | Ref _ ->
      Or_error.error_s
        [%message
          "Can't safely convert this address to an lvalue" ~address:(addr : t)]

let as_variable (addr : t) : Ac.C_id.t Or_error.t =
  Or_error.(addr |> as_lvalue >>= Lvalue.as_variable)

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Lvalue

  module On_monad (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let rec map_m x ~f =
      Variants.map x ~lvalue:(F.proc_variant1 f)
        ~ref:(F.proc_variant1 (map_m ~f))
  end
end)

module Type_check (E : Env_types.S) = struct
  module L = Lvalue.Type_check (E)

  let type_of : t -> Type.t Or_error.t =
    reduce ~lvalue:L.type_of ~ref:(Or_error.bind ~f:Type.ref)
end

let anonymise = function Lvalue v -> `A v | Ref d -> `B d

let deanonymise = function `A v -> Lvalue v | `B d -> Ref d

module Quickcheck_generic
    (Lv : Act_utils.My_quickcheck.S_with_sexp with type t := Lvalue.t) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end = struct
  open Base_quickcheck

  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Generator.t =
    Generator.(
      recursive_union
        [map [%quickcheck.generator: Lv.t] ~f:lvalue]
        ~f:(fun mu -> [map mu ~f:ref]))

  let quickcheck_observer : t Observer.t =
    Observer.(
      fixed_point (fun mu ->
          unmap ~f:anonymise
            [%quickcheck.observer: [`A of Lv.t | `B of [%custom mu]]]))

  let quickcheck_shrinker : t Shrinker.t =
    Shrinker.(
      fixed_point (fun mu ->
          map ~f:deanonymise ~f_inverse:anonymise
            [%quickcheck.shrinker: [`A of Lv.t | `B of [%custom mu]]]))
end

module Quickcheck_main = Quickcheck_generic (Lvalue)

include (Quickcheck_main : module type of Quickcheck_main with type t := t)

let on_address_of_typed_id ~(id : Ac.C_id.t) ~(ty : Type.t) : t =
  let lv = of_variable id in
  if Type.is_pointer ty then lv else ref lv

let of_id_in_env (env : Env.t) ~(id : Ac.C_id.t) : t Or_error.t =
  Or_error.Let_syntax.(
    let%map ty = Env.type_of env ~id in
    on_address_of_typed_id ~id ~ty)

let variable_of (addr : t) : Ac.C_id.t = Lvalue.variable_of (lvalue_of addr)

let variable_in_env (addr : t) ~(env : _ Map.M(Ac.C_id).t) : bool =
  Lvalue.variable_in_env (lvalue_of addr) ~env

let check_address_var (addr : t) ~(env : Env.t)
    : Act_common.C_id.t Or_error.t =
  let module A_check = Type_check (struct let env = env end) in
  Or_error.Let_syntax.(
    (* Addresses must have the same type as the entry for the variable in the
       environment. *)
    let id = variable_of addr in
    let%bind v_type = Env.type_of_known_value env ~id in
    let%bind a_type = A_check.type_of addr in
    let%map (_ : Type.t) =
      Or_error.tag_arg
        (Type.check v_type a_type)
        "Checking address var type" addr sexp_of_t
    in
    id)

let get_single_known_value (env : Env.t)
    (id : Act_common.C_id.t) : Constant.t Or_error.t =
  Or_error.(
    env
    |> Env.known_value ~id
    >>= Result.of_option
    ~error:(Error.of_string "env doesn't contain this value"))

let eval_on_env (addr : t) ~(env : Env.t) :
    Constant.t Or_error.t =
  Or_error.(addr |> check_address_var ~env >>= get_single_known_value env)
