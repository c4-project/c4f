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

module Base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let bmap (store : t) ~(src : Expression.t F.traversal)
      ~(dst : Address.t F.traversal) ~(mo : Mem_order.t F.traversal) : t M.t
      =
    Fields.fold ~init:(M.return store) ~src:(F.proc_field src)
      ~dst:(F.proc_field dst) ~mo:(F.proc_field mo)
end

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Address

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
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
    module B = Base_map (M)

    let map_m x ~f = B.bmap x ~src:f ~dst:M.return ~mo:M.return
  end
end)

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Lvalue

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module E = Expression_traverse.On_lvalues.On_monad (M)
    module A = Address.On_lvalues.On_monad (M)

    let map_m x ~f =
      B.bmap x ~src:(E.map_m ~f) ~dst:(A.map_m ~f) ~mo:M.return
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

module Quickcheck_ints (Src : Env_types.S_with_known_values) (Dst : Env_types.S_with_known_values) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t =
  Quickcheck_generic
    (Expression_gen.Int_values (Src)) (Address_gen.Atomic_int_pointers (Dst))

module Quickcheck_bools (Src : Env_types.S_with_known_values) (Dst : Env_types.S_with_known_values) :
  Act_utils.My_quickcheck.S_with_sexp with type t = t =
  Quickcheck_generic
    (Expression_gen.Bool_values
       (Src))
       (Address_gen.Atomic_bool_pointers (Dst))
