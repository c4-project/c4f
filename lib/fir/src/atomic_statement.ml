(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t =
  | Cmpxchg of Expression.t Atomic_cmpxchg.t
  | Fence of Atomic_fence.t
  | Fetch of Expression.t Atomic_fetch.t
  | Store of Atomic_store.t
[@@deriving accessors, sexp, compare, equal]

let value_map (type result) (x : t)
    ~(cmpxchg : Expression.t Atomic_cmpxchg.t -> result)
    ~(fence : Atomic_fence.t -> result)
    ~(fetch : Expression.t Atomic_fetch.t -> result)
    ~(store : Atomic_store.t -> result) : result =
  match x with
  | Cmpxchg c ->
      cmpxchg c
  | Fence f ->
      fence f
  | Fetch f ->
      fetch f
  | Store s ->
      store s

module Base_map (Ap : Applicative.S) = struct
  let bmap (x : t)
      ~(cmpxchg :
         Expression.t Atomic_cmpxchg.t -> Expression.t Atomic_cmpxchg.t Ap.t)
      ~(fence : Atomic_fence.t -> Atomic_fence.t Ap.t)
      ~(fetch :
         Expression.t Atomic_fetch.t -> Expression.t Atomic_fetch.t Ap.t)
      ~(store : Atomic_store.t -> Atomic_store.t Ap.t) : t Ap.t =
    Travesty_base_exts.Fn.Compose_syntax.(
      value_map x
        ~cmpxchg:(cmpxchg >> Ap.map ~f:(fun c -> Cmpxchg c))
        ~fence:(fence >> Ap.map ~f:(fun f -> Fence f))
        ~fetch:(fetch >> Ap.map ~f:(fun f -> Fetch f))
        ~store:(store >> Ap.map ~f:(fun s -> Store s)))
end

(** Does the legwork of implementing a particular type of traversal over
    statements. *)
module Make_traversal (Basic : sig
  module Elt : Equal.S

  (* There is no traversal over fences, as fences only have memory orders and
     nothing that we actually want to traverse here! *)
  module C :
    Travesty.Traversable_types.S0
      with type t := Expression.t Atomic_cmpxchg.t
       and module Elt = Elt

  module F :
    Travesty.Traversable_types.S0
      with type t := Expression.t Atomic_fetch.t
       and module Elt = Elt

  module N :
    Travesty.Traversable_types.S0
      with type t := Atomic_fence.t
       and module Elt = Elt

  module S :
    Travesty.Traversable_types.S0
      with type t := Atomic_store.t
       and module Elt = Elt
end) =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Basic.Elt

  module On (M : Applicative.S) = struct
    module SBase = Base_map (M)
    module C = Basic.C.On (M)
    module F = Basic.F.On (M)
    module N = Basic.N.On (M)
    module S = Basic.S.On (M)

    let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
      SBase.bmap x ~cmpxchg:(C.map_m ~f) ~fence:(N.map_m ~f)
        ~fetch:(F.map_m ~f) ~store:(S.map_m ~f)
  end
end)

module On_mem_orders :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Mem_order.t =
Make_traversal (struct
  module Elt = Mem_order
  module C = Expression_traverse.Cmpxchg.On_mem_orders
  module F = Expression_traverse.Fetch.On_mem_orders
  module N = Atomic_fence.On_mem_orders
  module S = Atomic_store.On_mem_orders
end)

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
Make_traversal (struct
  module Elt = Address
  module C = Expression_traverse.Cmpxchg.On_addresses
  module F = Expression_traverse.Fetch.On_addresses
  module N = Travesty.Traversable.Const (Atomic_fence) (Address)
  module S = Atomic_store.On_addresses
end)

module On_expressions :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Expression.t =
Make_traversal (struct
  module Elt = Expression
  module C =
    Travesty.Traversable.Fix_elt (Atomic_cmpxchg.On_expressions) (Expression)
  module F =
    Travesty.Traversable.Fix_elt (Atomic_fetch.On_expressions) (Expression)
  module N = Travesty.Traversable.Const (Atomic_fence) (Expression)
  module S = Atomic_store.On_expressions
end)
