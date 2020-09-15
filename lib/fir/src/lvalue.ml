(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type t = Variable of Common.C_id.t | Deref of t
[@@deriving sexp, accessors, compare, equal]

let of_variable_str_exn (s : string) : t =
  s |> Common.C_id.of_string |> Accessor.construct variable

let rec reduce (lv : t) ~(variable : Common.C_id.t -> 'a) ~(deref : 'a -> 'a)
    : 'a =
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
      Ok x

let rec get_variable_of (x : t) : Common.C_id.t =
  match x with Variable c -> c | Deref d -> get_variable_of d

let rec set_variable_of (x : t) (v : Common.C_id.t) : t =
  match x with
  | Variable _ ->
      Variable v
  | Deref d ->
      Deref (set_variable_of d v)

let variable_of : ('i, Common.C_id.t, t, [< field]) Accessor.Simple.t =
  [%accessor Accessor.field ~get:get_variable_of ~set:set_variable_of]

let as_variable (lv : t) : Common.C_id.t Or_error.t =
  Result.of_option lv.@?(variable)
    ~error:
      (Error.create_s
         [%message
           "Can't safely convert this lvalue to a variable" ~lvalue:(lv : t)])

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

let anonymise =
  [%accessor
    Accessor.isomorphism
      ~get:(function Variable v -> `A v | Deref d -> `B d)
      ~construct:(function `A v -> Variable v | `B d -> Deref d)]

module Quickcheck_generic
    (Id : Act_utils.My_quickcheck.S_with_sexp with type t := Common.C_id.t) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Q.Generator.t =
    Q.Generator.(
      recursive_union
        [map [%quickcheck.generator: Id.t] ~f:(Accessor.construct variable)]
        ~f:(fun mu -> [map mu ~f:(Accessor.construct deref)]))

  let quickcheck_observer : t Q.Observer.t =
    Q.Observer.(
      fixed_point (fun mu ->
          unmap ~f:(Accessor.get anonymise)
            [%quickcheck.observer: [`A of Id.t | `B of [%custom mu]]]))

  let quickcheck_shrinker : t Q.Shrinker.t =
    Q.Shrinker.(
      fixed_point (fun mu ->
          map
            ~f:(Accessor.construct anonymise)
            ~f_inverse:(Accessor.get anonymise)
            [%quickcheck.shrinker: [`A of Id.t | `B of [%custom mu]]]))
end

module Quickcheck_id = Quickcheck_generic (Common.C_id)

include (Quickcheck_id : module type of Quickcheck_id with type t := t)

let on_value_of_typed_id (tid : Type.t Common.C_named.t) : t =
  let id = Accessor.get Common.C_named.name tid in
  let ty = Accessor.get Common.C_named.value tid in
  if Type.is_pointer ty then Deref (Variable id) else Variable id
