(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(* Needed because Base shadows it: *)
module Ty = Type

open Base
open Import

module M = struct
  type t = Lvalue of Lvalue.t | Ref of t
  [@@deriving sexp, accessors, compare, equal]
end

include M
include Comparable.Make (M)

let variable : ('i, Common.C_id.t, t, [< variant]) Accessor.t =
  [%accessor lvalue @> Lvalue.variable]

let variable_ref : ('i, Common.C_id.t, t, [< variant]) Accessor.t =
  [%accessor ref @> variable]

let of_variable_str_exn (vs : string) : t =
  Lvalue (Lvalue.of_variable_str_exn vs)

let ref_lvalue (l : Lvalue.t) : t =
  match Lvalue.un_deref l with
  | Ok l' -> Lvalue l'
  | Error _ -> Ref (Lvalue l)

let ref_normal : t -> t = function Lvalue k -> ref_lvalue k | x -> Ref x

let rec reduce (addr : t) ~(lvalue : Lvalue.t -> 'a) ~(ref : 'a -> 'a) : 'a =
  match addr with
  | Lvalue lv -> lvalue lv
  | Ref rest -> ref (reduce rest ~lvalue ~ref)

let lvalue_of : ('i, Lvalue.t, t, [< field]) Accessor.t =
  [%accessor
    Accessor.field ~get:(reduce ~lvalue:Fn.id ~ref:Fn.id) ~set:(fun x v ->
        reduce x ~lvalue:(fun _ -> Lvalue v) ~ref:(fun x -> Ref x) )]

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Lvalue

  module On (M : Applicative.S) = struct
    module AccM = Accessor.Of_applicative (M)

    let map_m : t -> f:(Elt.t -> Elt.t M.t) -> t M.t = AccM.map lvalue_of
  end
end)

let ref_depth = reduce ~lvalue:(Fn.const 0) ~ref:Int.succ

let normalise (addr : t) : t =
  Fn.apply_n_times ~n:(ref_depth addr) ref_normal (Lvalue addr.@(lvalue_of))

let deref (addr : t) : t =
  match normalise addr with
  | Ref x -> x
  | Lvalue l -> Lvalue (Accessor.construct Lvalue.deref l)

let as_lvalue (addr : t) : Lvalue.t Or_error.t =
  match normalise addr with
  | Lvalue x -> Ok x
  | Ref _ ->
      Or_error.error_s
        [%message
          "Can't safely convert this address to an lvalue" ~address:(addr : t)]

let as_variable (addr : t) : Common.C_id.t Or_error.t =
  Or_error.(addr |> as_lvalue >>= Lvalue.as_variable)

module Type_check (E : Env_types.S) = struct
  module L = Lvalue.Type_check (E)

  let type_of : t -> Ty.t Or_error.t =
    reduce ~lvalue:L.type_of ~ref:(Or_error.bind ~f:Ty.ref)
end

let anonymise = function Lvalue v -> `A v | Ref d -> `B d

let deanonymise = function `A v -> Lvalue v | `B d -> Ref d

module Quickcheck_generic
    (Lv : C4f_utils.My_quickcheck.S_with_sexp with type t := Lvalue.t) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end = struct
  open Base_quickcheck

  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Generator.t =
    Generator.(
      recursive_union
        [map [%quickcheck.generator: Lv.t] ~f:(Accessor.construct lvalue)]
        ~f:(fun mu -> [map mu ~f:(Accessor.construct M.ref)]))

  let quickcheck_observer : t Observer.t =
    Observer.(
      fixed_point (fun mu ->
          unmap ~f:anonymise
            [%quickcheck.observer: [`A of Lv.t | `B of [%custom mu]]] ))

  let quickcheck_shrinker : t Shrinker.t =
    Shrinker.(
      fixed_point (fun mu ->
          map ~f:deanonymise ~f_inverse:anonymise
            [%quickcheck.shrinker: [`A of Lv.t | `B of [%custom mu]]] ))
end

module Quickcheck_main = Quickcheck_generic (Lvalue)

include (Quickcheck_main : module type of Quickcheck_main with type t := t)

let on_address_of_typed_id (tid : Ty.t Common.C_named.t) : t =
  let id = tid.@(Common.C_named.name) in
  let ty = tid.@(Common.C_named.value) in
  let lv = Accessor.construct variable id in
  if Ty.is_pointer ty then lv else Ref lv

let of_id_in_env (env : Env.t) ~(id : Common.C_id.t) : t Or_error.t =
  Or_error.Let_syntax.(
    let%map ty = Env.type_of env ~id in
    on_address_of_typed_id (Common.C_named.make ~name:id ty))

let variable_of : ('i, Common.C_id.t, t, [< field]) Accessor.t =
  [%accessor lvalue_of @> Lvalue.variable_of]

let check_address_var (addr : t) ~(env : Env.t) :
    C4f_common.C_id.t Or_error.t =
  let module A_check = Type_check (struct
    let env = env
  end) in
  Or_error.Let_syntax.(
    (* Addresses must have the same type as the entry for the variable in the
       environment. *)
    let id = addr.@(variable_of) in
    let%bind v_type = Env.type_of_known_value env ~id in
    let%bind a_type = A_check.type_of addr in
    let%map (_ : Ty.t) =
      Or_error.tag_arg
        (Ty.check v_type a_type)
        "Checking address var type" addr sexp_of_t
    in
    id)

let get_single_known_value (env : Env.t) (id : C4f_common.C_id.t) :
    Constant.t Or_error.t =
  Or_error.(
    env |> Env.known_value ~id
    >>= Result.of_option
          ~error:(Error.of_string "env doesn't contain this value"))

let eval_on_env (addr : t) ~(env : Env.t) : Constant.t Or_error.t =
  Or_error.(addr |> check_address_var ~env >>= get_single_known_value env)
