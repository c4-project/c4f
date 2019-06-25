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

open Base
module Ac = Act_common

type t = Lvalue of Mini_lvalue.t | Ref of t
[@@deriving sexp, variants, equal]

let of_variable (v : Ac.C_id.t) : t = Lvalue (Mini_lvalue.variable v)

let of_variable_ref (v : Ac.C_id.t) : t = Ref (of_variable v)

let ref_lvalue (l : Mini_lvalue.t) : t =
  match Mini_lvalue.un_deref l with
  | Ok l' ->
      lvalue l'
  | Error _ ->
      ref (lvalue l)

let ref_normal : t -> t = function Lvalue k -> ref_lvalue k | x -> ref x

let rec reduce (addr : t) ~(lvalue : Mini_lvalue.t -> 'a) ~(ref : 'a -> 'a)
    : 'a =
  match addr with
  | Lvalue lv ->
      lvalue lv
  | Ref rest ->
      ref (reduce rest ~lvalue ~ref)

let lvalue_of : t -> Mini_lvalue.t = reduce ~lvalue:Fn.id ~ref:Fn.id

let ref_depth = reduce ~lvalue:(Fn.const 0) ~ref:Int.succ

let normalise (addr : t) : t =
  Fn.apply_n_times ~n:(ref_depth addr) ref_normal (lvalue (lvalue_of addr))

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Mini_lvalue.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Mini_lvalue

  module On_monad (M : Monad.S) = struct
    module F = Travesty.Traversable.Helpers (M)

    let rec map_m x ~f =
      Variants.map x ~lvalue:(F.proc_variant1 f)
        ~ref:(F.proc_variant1 (map_m ~f))
  end
end)

module Type_check (E : Mini_env.S) = struct
  module L = Mini_lvalue.Type_check (E)

  let type_of : t -> Mini_type.t Or_error.t =
    reduce ~lvalue:L.type_of ~ref:(Or_error.bind ~f:Mini_type.ref)
end

let anonymise = function Lvalue v -> `A v | Ref d -> `B d

let deanonymise = function `A v -> Lvalue v | `B d -> Ref d

module Quickcheck_generic
    (Lv : Act_utils.My_quickcheck.S_with_sexp with type t := Mini_lvalue.t) : sig
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
            [%quickcheck.observer: [`A of Lv.t | `B of [%custom mu]]] ))

  let quickcheck_shrinker : t Shrinker.t =
    Shrinker.(
      fixed_point (fun mu ->
          map ~f:deanonymise ~f_inverse:anonymise
            [%quickcheck.shrinker: [`A of Lv.t | `B of [%custom mu]]] ))
end

module Quickcheck_main = Quickcheck_generic (Mini_lvalue)

include (Quickcheck_main : module type of Quickcheck_main with type t := t)

let on_address_of_typed_id ~(id : Ac.C_id.t) ~(ty : Mini_type.t) : t =
  let lv = of_variable id in
  if Mini_type.is_pointer ty then lv else ref lv

let variable_of (addr : t) : Ac.C_id.t =
  Mini_lvalue.variable_of (lvalue_of addr)

let variable_in_env (addr : t) ~(env : _ Ac.C_id.Map.t) : bool =
  Mini_lvalue.variable_in_env (lvalue_of addr) ~env

module Quickcheck_on_env (E : Mini_env.S) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end =
  Quickcheck_generic (Mini_lvalue.Quickcheck_on_env (E))

module Quickcheck_atomic_int_pointers (E : Mini_env.S) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end = struct
  open Base_quickcheck

  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Generator.t =
    Generator.map
      (Generator.of_list (Map.to_alist (E.atomic_int_variables ())))
      ~f:(fun (id, ty) -> on_address_of_typed_id ~id ~ty)

  module Q = Quickcheck_on_env (E)

  let quickcheck_observer = [%quickcheck.observer: Q.t]

  let quickcheck_shrinker = [%quickcheck.shrinker: Q.t]
end
