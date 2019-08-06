(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

open Core_kernel (* for Quickcheck.test_distinct_values *)

module Ac = Act_common

type t = Variable of Ac.C_id.t | Deref of t
[@@deriving sexp, variants, compare, equal]

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

let variable_in_env (lv : t) ~(env : _ Ac.C_id.Map.t) : bool =
  Map.mem env (variable_of lv)

let as_variable (lv : t) : Ac.C_id.t Or_error.t =
  match lv with
  | Variable x ->
      Or_error.return x
  | Deref _ ->
      Or_error.error_s
        [%message
          "Can't safely convert this lvalue to a variable" ~lvalue:(lv : t)]

module Type_check (E : Env_types.S) = struct
  let type_of_variable (v : Ac.C_id.t) : Type.t Or_error.t =
    Result.of_option (Map.find E.env v)
      ~error:
        (Error.create_s
           [%message
             "Variable not in environment"
               ~variable:(v : Ac.C_id.t)
               ~environment:(E.env : Type.t Map.M(Ac.C_id).t)])

  let rec type_of : t -> Type.t Or_error.t = function
    | Variable v ->
        type_of_variable v
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

let on_value_of_typed_id ~(id : Ac.C_id.t) ~(ty : Type.t) : t =
  if Type.is_pointer ty then Deref (Variable id) else Variable id
