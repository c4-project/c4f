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
    | Cmpxchg of Expression.t Atomic_cmpxchg.t
    | Fence of Atomic_fence.t
    | Fetch of Expression.t Atomic_fetch.t
    | Store of Atomic_store.t
  [@@deriving variants, sexp, compare, equal]
end

include P

let reduce (type result) (x : t)
    ~(cmpxchg : Expression.t Atomic_cmpxchg.t -> result)
    ~(fence : Atomic_fence.t -> result)
    ~(fetch : Expression.t Atomic_fetch.t -> result)
    ~(store : Atomic_store.t -> result) : result =
  Variants.map x ~cmpxchg:(Fn.const cmpxchg) ~fence:(Fn.const fence)
    ~fetch:(Fn.const fetch) ~store:(Fn.const store)

module Base_map (Ap : Applicative.S) = struct
  let bmap (x : t)
      ~(cmpxchg :
         Expression.t Atomic_cmpxchg.t -> Expression.t Atomic_cmpxchg.t Ap.t)
      ~(fence : Atomic_fence.t -> Atomic_fence.t Ap.t)
      ~(fetch :
         Expression.t Atomic_fetch.t -> Expression.t Atomic_fetch.t Ap.t)
      ~(store : Atomic_store.t -> Atomic_store.t Ap.t) : t Ap.t =
    Travesty_base_exts.Fn.Compose_syntax.(
      reduce x
        ~cmpxchg:(cmpxchg >> Ap.map ~f:P.cmpxchg)
        ~fence:(fence >> Ap.map ~f:P.fence)
        ~fetch:(fetch >> Ap.map ~f:P.fetch)
        ~store:(store >> Ap.map ~f:P.store))
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

  module S :
    Travesty.Traversable_types.S0
      with type t := Atomic_store.t
       and module Elt = Elt
end) =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Basic.Elt

  module On_monad (M : Monad.S) = struct
    module SBase = Base_map (struct
      type 'a t = 'a M.t

      include Applicative.Of_monad (M)
    end)

    module C = Basic.C.On_monad (M)
    module F = Basic.F.On_monad (M)
    module S = Basic.S.On_monad (M)

    let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
      SBase.bmap x ~cmpxchg:(C.map_m ~f) ~fence:M.return ~fetch:(F.map_m ~f)
        ~store:(S.map_m ~f)
  end
end)

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
Make_traversal (struct
  module Elt = Lvalue
  module C = Expression_traverse.Cmpxchg.On_lvalues
  module F = Expression_traverse.Fetch.On_lvalues
  module S = Atomic_store.On_lvalues
end)

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
Make_traversal (struct
  module Elt = Address
  module C = Expression_traverse.Cmpxchg.On_addresses
  module F = Expression_traverse.Fetch.On_addresses
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
  module S = Atomic_store.On_expressions
end)
