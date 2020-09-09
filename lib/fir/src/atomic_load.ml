(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
module Ac = Act_common

type t = {src: Address.t; mo: Mem_order.t}
[@@deriving sexp, fields, make, compare, equal]

let to_tuple ({src; mo} : t) : Address.t * Mem_order.t = (src, mo)

let of_tuple ((src, mo) : Address.t * Mem_order.t) : t = {src; mo}

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

  module On_monad (M : Monad.S) = struct
    module B = Base_map (struct
      type 'a t = 'a M.t

      include Applicative.Of_monad (M)
    end)

    let map_m x ~f = B.bmap x ~src:f ~mo:M.return
  end
end)

module On_mem_orders :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Mem_order.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Mem_order

  module On_monad (M : Monad.S) = struct
    module B = Base_map (struct
      type 'a t = 'a M.t

      include Applicative.Of_monad (M)
    end)

    let map_m x ~f = B.bmap x ~src:M.return ~mo:f
  end
end)

module Type_check (E : Env_types.S) = struct
  module A = Address.Type_check (E)

  let type_of (ld : t) : Type.t Or_error.t =
    let open Or_error.Let_syntax in
    let%bind a_ptr = A.type_of (src ld) in
    let%bind a = Type.deref a_ptr in
    Type.to_non_atomic a
end

module Quickcheck_generic
    (A : Act_utils.My_quickcheck.S_with_sexp with type t := Address.t) : sig
  type nonrec t = t [@@deriving sexp_of, quickcheck]
end = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.map ~f:of_tuple
      [%quickcheck.generator: A.t * [%custom Mem_order.gen_load]]

  let quickcheck_observer : t Base_quickcheck.Observer.t =
    Base_quickcheck.Observer.unmap ~f:to_tuple
      [%quickcheck.observer: A.t * Mem_order.t]

  let quickcheck_shrinker : t Base_quickcheck.Shrinker.t =
    Base_quickcheck.Shrinker.map ~f:of_tuple ~f_inverse:to_tuple
      [%quickcheck.shrinker: A.t * Mem_order.t]
end

module Quickcheck_main = Quickcheck_generic (Address)

include (Quickcheck_main : module type of Quickcheck_main with type t := t)

let variable_of (ld : t) : Ac.C_id.t = Address.variable_of (src ld)
