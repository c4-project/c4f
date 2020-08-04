(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common

type t = Variable of Ac.C_id.t | Deref of t
[@@deriving sexp, variants, compare, equal]

let of_variable_str_exn (s : string) : t =
  s |> Act_common.C_id.of_string |> variable

let rec reduce (lv : t) ~(variable : Ac.C_id.t -> 'a) ~(deref : 'a -> 'a) :
    'a =
  match lv with
  | Variable v ->
      variable v
  | Deref rest ->
      deref (reduce rest ~variable ~deref)

let is_deref : t -> bool = function Deref _ -> true | Variable _ -> false

let un_deref : t -> t Or_error.t = function
  | Variable _ ->
      Or_error.error_string "can't & a variable lvalue"
  | Deref x ->
      Or_error.return x

module On_identifiers :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Ac.C_id.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Ac.C_id

  module On_monad (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let rec map_m x ~f =
      Variants.map x ~variable:(F.proc_variant1 f)
        ~deref:(F.proc_variant1 (map_m ~f))
  end
end)

let variable_of : t -> Ac.C_id.t = reduce ~variable:Fn.id ~deref:Fn.id

let as_variable (lv : t) : Ac.C_id.t Or_error.t =
  match lv with
  | Variable x ->
      Or_error.return x
  | Deref _ ->
      Or_error.error_s
        [%message
          "Can't safely convert this lvalue to a variable" ~lvalue:(lv : t)]

module Type_check (E : sig
  val env : Env.t
end) =
struct
  let rec type_of : t -> Type.t Or_error.t = function
    | Variable id ->
        Env.type_of E.env ~id
    | Deref l ->
        Or_error.tag_arg
          Or_error.(l |> type_of >>= Type.deref)
          "While checking underlying type of lvalue dereferencing:" l
          sexp_of_t
end

let anonymise = function Variable v -> `A v | Deref d -> `B d

let deanonymise = function `A v -> Variable v | `B d -> Deref d

module Quickcheck_generic
    (Id : Act_utils.My_quickcheck.S_with_sexp with type t := Ac.C_id.t) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.(
      recursive_union
        [map [%quickcheck.generator: Id.t] ~f:variable]
        ~f:(fun mu -> [map mu ~f:deref]))

  let quickcheck_observer : t Base_quickcheck.Observer.t =
    Base_quickcheck.Observer.(
      fixed_point (fun mu ->
          unmap ~f:anonymise
            [%quickcheck.observer: [`A of Id.t | `B of [%custom mu]]]))

  let quickcheck_shrinker : t Base_quickcheck.Shrinker.t =
    Base_quickcheck.Shrinker.(
      fixed_point (fun mu ->
          map ~f:deanonymise ~f_inverse:anonymise
            [%quickcheck.shrinker: [`A of Id.t | `B of [%custom mu]]]))
end

module Quickcheck_id = Quickcheck_generic (Ac.C_id)

include (Quickcheck_id : module type of Quickcheck_id with type t := t)

let on_value_of_typed_id (tid : Type.t Ac.C_named.t) : t =
  let id = Ac.C_named.name tid in
  let ty = Ac.C_named.value tid in
  if Type.is_pointer ty then Deref (Variable id) else Variable id
