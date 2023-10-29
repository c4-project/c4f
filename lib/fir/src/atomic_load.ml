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

type t = {src: Address.t; mo: Mem_order.t}
[@@deriving sexp, accessors, make, compare, equal]

let tuple : ('i, Address.t * Mem_order.t, t, [< isomorphism]) Accessor.t =
  [%accessor
    Accessor.isomorphism
      ~get:(fun {src; mo} -> (src, mo))
      ~construct:(fun (src, mo) -> {src; mo})]

let ensure_mo_compat (old : Mem_order.t) (nu : Mem_order.t) : Mem_order.t =
  if Mem_order.is_load_compatible nu then nu else old

module Base_map (Ap : Applicative.S) = struct
  let bmap (x : t) ~(src : Address.t -> Address.t Ap.t)
      ~(mo : Mem_order.t -> Mem_order.t Ap.t) : t Ap.t =
    Ap.(
      let m src mo = make ~src ~mo:(ensure_mo_compat x.mo mo) in
      return m <*> src x.src <*> mo x.mo)
end

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Address

  module On (M : Applicative.S) = struct
    module AccM = Accessor.Of_applicative (M)

    let map_m : t -> f:(Address.t -> Address.t M.t) -> t M.t = AccM.map src
  end
end)

module On_mem_orders :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Mem_order.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Mem_order

  module On (M : Applicative.S) = struct
    module B = Base_map (M)

    let map_m x ~f = B.bmap x ~src:M.return ~mo:f
  end
end)

module Type_check (E : Env_types.S) = struct
  module A = Address.Type_check (E)

  let type_of (ld : t) : Ty.t Or_error.t =
    Or_error.Let_syntax.(
      let%bind a_ptr = A.type_of ld.src in
      let%bind a = Ty.deref a_ptr in
      Ty.to_non_atomic a)
end

module Quickcheck_generic
    (A : C4f_utils.My_quickcheck.S_with_sexp with type t := Address.t) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.map
      ~f:(Accessor.construct tuple)
      [%quickcheck.generator: A.t * [%custom Mem_order.gen_load]]

  let quickcheck_observer : t Base_quickcheck.Observer.t =
    Base_quickcheck.Observer.unmap ~f:(Accessor.get tuple)
      [%quickcheck.observer: A.t * Mem_order.t]

  let quickcheck_shrinker : t Base_quickcheck.Shrinker.t =
    Base_quickcheck.Shrinker.map
      ~f:(Accessor.construct tuple)
      ~f_inverse:(Accessor.get tuple)
      [%quickcheck.shrinker: A.t * Mem_order.t]
end

module Quickcheck_main = Quickcheck_generic (Address)

include (Quickcheck_main : module type of Quickcheck_main with type t := t)

let variable_of : type i. (i, Common.C_id.t, t, [< field]) Accessor.t =
  [%accessor src @> Address.variable_of]
