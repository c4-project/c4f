(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = {src: Expression.t; dst: Address.t; mo: Mem_order.t}
[@@deriving sexp, fields, make, compare, equal]

let to_tuple ({src; dst; mo} : t) : Expression.t * Address.t * Mem_order.t =
  (src, dst, mo)

let of_tuple ((src, dst, mo) : Expression.t * Address.t * Mem_order.t) : t =
  {src; dst; mo}

let quickcheck_observer : t Base_quickcheck.Observer.t =
  Base_quickcheck.Observer.(
    unmap [%quickcheck.observer: Expression.t * Address.t * Mem_order.t]
      ~f:to_tuple)

let ensure_mo_compat (old : Mem_order.t) (nu : Mem_order.t) : Mem_order.t =
  if Mem_order.is_store_compatible nu then nu else old

module Base_map (Ap : Applicative.S) = struct
  let bmap (x : t) ~(src : Expression.t -> Expression.t Ap.t)
      ~(dst : Address.t -> Address.t Ap.t)
      ~(mo : Mem_order.t -> Mem_order.t Ap.t) : t Ap.t =
    Ap.(
      let m src dst mo = make ~src ~dst ~mo:(ensure_mo_compat x.mo mo) in
      return m <*> src x.src <*> dst x.dst <*> mo x.mo)
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

    module E = Expression_traverse.On_addresses.On_monad (M)

    let map_m x ~f = B.bmap x ~src:(E.map_m ~f) ~dst:f ~mo:M.return
  end
end)

module On_expressions :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Expression.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Expression

  module On_monad (M : Monad.S) = struct
    module B = Base_map (struct
      type 'a t = 'a M.t

      include Applicative.Of_monad (M)
    end)

    let map_m x ~f = B.bmap x ~src:f ~dst:M.return ~mo:M.return
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

    module E = Expression_traverse.On_mem_orders.On_monad (M)

    let map_m x ~f = B.bmap x ~src:(E.map_m ~f) ~dst:M.return ~mo:f
  end
end)

module Quickcheck_generic
    (Src : Act_utils.My_quickcheck.S_with_sexp with type t := Expression.t)
    (Dst : Act_utils.My_quickcheck.S_with_sexp with type t := Address.t) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t = struct
  type nonrec t = t

  let sexp_of_t = sexp_of_t

  let quickcheck_generator : t Base_quickcheck.Generator.t =
    Base_quickcheck.Generator.(
      map
        [%quickcheck.generator:
          Src.t * Dst.t * [%custom Mem_order.gen_store]] ~f:of_tuple)

  let quickcheck_observer : t Base_quickcheck.Observer.t =
    quickcheck_observer

  let quickcheck_shrinker : t Base_quickcheck.Shrinker.t =
    Base_quickcheck.Shrinker.(
      map [%quickcheck.shrinker: Src.t * Dst.t * Mem_order.t] ~f:of_tuple
        ~f_inverse:to_tuple)
end

module Quickcheck_ints (Src : Env_types.S) (Dst : Env_types.S) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t =
  Quickcheck_generic
    (Expression_gen.Int_values (Src)) (Address_gen.Atomic_int_pointers (Dst))

module Quickcheck_bools (Src : Env_types.S) (Dst : Env_types.S) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t =
  Quickcheck_generic
    (Expression_gen.Bool_values
       (Src))
       (Address_gen.Atomic_bool_pointers (Dst))
