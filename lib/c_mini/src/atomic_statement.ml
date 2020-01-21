(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(* This module wrapper exists to let us refer to the generated variant
   constructors in Base_map. *)
module P = struct
  type t =
    | Atomic_cmpxchg of Atomic_cmpxchg.t
    | Atomic_fence of Atomic_fence.t
    | Atomic_store of Atomic_store.t
  [@@deriving variants, sexp, compare, equal]
end

include P

let reduce (type result) (x : t)
    ~(atomic_cmpxchg : Atomic_cmpxchg.t -> result)
    ~(atomic_fence : Atomic_fence.t -> result)
    ~(atomic_store : Atomic_store.t -> result) : result =
  Variants.map x ~atomic_fence:(Fn.const atomic_fence)
    ~atomic_store:(Fn.const atomic_store)
    ~atomic_cmpxchg:(Fn.const atomic_cmpxchg)

module Base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let bmap (x : t)
      ~(atomic_cmpxchg : Atomic_cmpxchg.t -> Atomic_cmpxchg.t M.t)
      ~(atomic_fence : Atomic_fence.t -> Atomic_fence.t M.t)
      ~(atomic_store : Atomic_store.t -> Atomic_store.t M.t) : t M.t =
    Travesty_base_exts.Fn.Compose_syntax.(
      reduce x
        ~atomic_cmpxchg:(atomic_cmpxchg >> M.map ~f:P.atomic_cmpxchg)
        ~atomic_fence:(atomic_fence >> M.map ~f:P.atomic_fence)
        ~atomic_store:(atomic_store >> M.map ~f:P.atomic_store))
end

(** Does the legwork of implementing a particular type of traversal over
    statements. *)
module Make_traversal (Basic : sig
  module Elt : Equal.S

  (* There is no traversal over fences, as fences only have memory orders and
     nothing that we actually want to traverse here! *)
  module X :
    Travesty.Traversable_types.S0
      with type t := Atomic_cmpxchg.t
       and module Elt = Elt

  module S :
    Travesty.Traversable_types.S0
      with type t := Atomic_store.t
       and module Elt = Elt
end) =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Basic.Elt

  module On_monad (M : Monad.S) = struct
    module SBase = Base_map (M)
    module SM = Basic.S.On_monad (M)
    module XM = Basic.X.On_monad (M)

    let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
      SBase.bmap x ~atomic_fence:M.return ~atomic_store:(SM.map_m ~f)
        ~atomic_cmpxchg:(XM.map_m ~f)
  end
end)

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
Make_traversal (struct
  module Elt = Lvalue
  module S = Atomic_store.On_lvalues
  module X = Atomic_cmpxchg.On_lvalues
end)

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
Make_traversal (struct
  module Elt = Address
  module S = Atomic_store.On_addresses
  module X = Atomic_cmpxchg.On_addresses
end)
