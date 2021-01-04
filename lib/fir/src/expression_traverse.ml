(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(* As explained in the interface file, this module is quite convoluted in its
   design; this is because we have to try to eliminate as much module level
   recursion as possible. Instead, we try keep the recursion at the function
   level in [Make_traversal_base], and then extrude all the various 'proper'
   traversals out of it. *)

module type Basic = sig
  module Elt : Equal.S

  module A :
    Travesty.Traversable_types.S0
      with type t := Address.t
       and module Elt = Elt

  module C :
    Travesty.Traversable_types.S0
      with type t := Constant.t
       and module Elt = Elt

  module L :
    Travesty.Traversable_types.S0
      with type t := Atomic_load.t
       and module Elt = Elt

  module O :
    Travesty.Traversable_types.S0
      with type t := Mem_order.t
       and module Elt = Elt
end

module Make_traversal_base (Basic : Basic) = struct
  type t = Expression.t

  module Elt = Basic.Elt

  module On (M : Applicative.S) = struct
    module A = Basic.A.On (M)
    module C = Basic.C.On (M)
    module L = Basic.L.On (M)
    module O = Basic.O.On (M)
    module AC = Atomic_cmpxchg.Base_map (M)
    module AF = Atomic_fetch.Base_map (M)
    module AX = Atomic_xchg.Base_map (M)

    let map_m_cmpxchg (x : t Atomic_cmpxchg.t) ~(f : Elt.t -> Elt.t M.t)
        ~(mu : t -> t M.t) : t Atomic_cmpxchg.t M.t =
      AC.bmap x ~obj:(A.map_m ~f) ~expected:(A.map_m ~f) ~desired:mu
        ~strength:M.return (* for now *) ~succ:(O.map_m ~f)
        ~fail:(O.map_m ~f)

    let map_m_fetch (x : t Atomic_fetch.t) ~(f : Elt.t -> Elt.t M.t)
        ~(mu : t -> t M.t) : t Atomic_fetch.t M.t =
      AF.bmap x ~obj:(A.map_m ~f) ~arg:mu ~mo:(O.map_m ~f) ~op:M.return

    let map_m_xchg (x : t Atomic_xchg.t) ~(f : Elt.t -> Elt.t M.t)
        ~(mu : t -> t M.t) : t Atomic_xchg.t M.t =
      AX.bmap x ~obj:(A.map_m ~f) ~desired:mu ~mo:(O.map_m ~f)

    let map_m_ae (ae : t Atomic_expression.t) ~(f : Elt.t -> Elt.t M.t)
        ~(mu : t -> t M.t) : t Atomic_expression.t M.t =
      Travesty_base_exts.Fn.Compose_syntax.(
        Atomic_expression.(
          reduce ae
            ~cmpxchg:(map_m_cmpxchg ~f ~mu >> M.map ~f:cmpxchg)
            ~fetch:(map_m_fetch ~f ~mu >> M.map ~f:fetch)
            ~load:(L.map_m ~f >> M.map ~f:load)
            ~xchg:(map_m_xchg ~f ~mu >> M.map ~f:xchg)))

    module B = Expression.Base_map (M)
    module AccM = Accessor.Of_applicative (M)

    let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
      let rec mu x =
        (* We could use reduce, but that'd preclude us being able to express
           map_m_ae et al. separately. *)
        B.bmap x ~constant:(C.map_m ~f) ~address:(A.map_m ~f)
          ~atomic:(map_m_ae ~f ~mu)
          ~uop:(fun (u, x) -> M.map ~f:(fun x' -> (u, x')) (mu x))
          ~bop:(fun (u, l, r) ->
            M.map2 ~f:(fun l' r' -> (u, l', r')) (mu l) (mu r))
          ~ternary:(AccM.map ~f:mu Expr_ternary.exprs)
      in
      mu x
  end
end

module Make_traversal (Basic : Basic) =
  Travesty.Traversable.Make0 (Make_traversal_base (Basic))

module On_addresses :
  Travesty.Traversable_types.S0
    with type t = Expression.t
     and type Elt.t = Address.t = Make_traversal (struct
  module Elt = Address
  module A =
    Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (Address)
  module C = Travesty.Traversable.Const (Constant) (Address)
  module L = Atomic_load.On_addresses
  module O = Travesty.Traversable.Const (Mem_order) (Address)
end)

module On_constants :
  Travesty.Traversable_types.S0
    with type t = Expression.t
     and type Elt.t = Constant.t = Make_traversal (struct
  module Elt = Constant
  module A = Travesty.Traversable.Const (Address) (Constant)
  module C =
    Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (Constant)
  module L = Travesty.Traversable.Const (Atomic_load) (Constant)
  module O = Travesty.Traversable.Const (Mem_order) (Constant)
end)

module On_mem_orders :
  Travesty.Traversable_types.S0
    with type t = Expression.t
     and type Elt.t = Mem_order.t = Make_traversal (struct
  module Elt = Mem_order
  module A = Travesty.Traversable.Const (Address) (Mem_order)
  module C = Travesty.Traversable.Const (Constant) (Mem_order)
  module L = Atomic_load.On_mem_orders
  module O =
    Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (Mem_order)
end)

module Cmpxchg :
  Expression_types.S_traversable with type t = Expression.t Atomic_cmpxchg.t =
struct
  type t = Expression.t Atomic_cmpxchg.t

  module Make_traversal (Basic : Basic) = Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Basic.Elt
    module TB = Make_traversal_base (Basic)

    module On (M : Applicative.S) = struct
      module TM = TB.On (M)

      let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
        TM.map_m_cmpxchg x ~f ~mu:(TM.map_m ~f)
    end
  end)

  module On_addresses :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Make_traversal (struct
    module Elt = Address
    module A =
      Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (Address)
    module C = Travesty.Traversable.Const (Constant) (Address)
    module L = Atomic_load.On_addresses
    module O = Travesty.Traversable.Const (Mem_order) (Address)
  end)

  module On_constants :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Constant.t =
  Make_traversal (struct
    module Elt = Constant
    module A = Travesty.Traversable.Const (Address) (Constant)
    module C =
      Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (Constant)
    module L = Travesty.Traversable.Const (Atomic_load) (Constant)
    module O = Travesty.Traversable.Const (Mem_order) (Constant)
  end)

  module On_mem_orders :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Mem_order.t = Make_traversal (struct
    module Elt = Mem_order
    module A = Travesty.Traversable.Const (Address) (Mem_order)
    module C = Travesty.Traversable.Const (Constant) (Mem_order)
    module L = Atomic_load.On_mem_orders
    module O =
      Travesty.Traversable.Fix_elt
        (Travesty_containers.Singleton)
        (Mem_order)
  end)
end

module Fetch :
  Expression_types.S_traversable with type t = Expression.t Atomic_fetch.t =
struct
  type t = Expression.t Atomic_fetch.t

  module Make_traversal (Basic : Basic) = Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Basic.Elt
    module TB = Make_traversal_base (Basic)

    module On (M : Applicative.S) = struct
      module TM = TB.On (M)

      let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
        TM.map_m_fetch x ~f ~mu:(TM.map_m ~f)
    end
  end)

  module On_addresses :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Make_traversal (struct
    module Elt = Address
    module A =
      Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (Address)
    module C = Travesty.Traversable.Const (Constant) (Address)
    module L = Atomic_load.On_addresses
    module O = Travesty.Traversable.Const (Mem_order) (Address)
  end)

  module On_constants :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Constant.t =
  Make_traversal (struct
    module Elt = Constant
    module A = Travesty.Traversable.Const (Address) (Constant)
    module C =
      Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (Constant)
    module L = Travesty.Traversable.Const (Atomic_load) (Constant)
    module O = Travesty.Traversable.Const (Mem_order) (Constant)
  end)

  module On_mem_orders :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Mem_order.t = Make_traversal (struct
    module Elt = Mem_order
    module A = Travesty.Traversable.Const (Address) (Mem_order)
    module C = Travesty.Traversable.Const (Constant) (Mem_order)
    module L = Atomic_load.On_mem_orders
    module O =
      Travesty.Traversable.Fix_elt
        (Travesty_containers.Singleton)
        (Mem_order)
  end)
end

module Xchg :
  Expression_types.S_traversable with type t = Expression.t Atomic_xchg.t =
struct
  type t = Expression.t Atomic_xchg.t

  module Make_traversal (Basic : Basic) = Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Basic.Elt
    module TB = Make_traversal_base (Basic)

    module On (M : Applicative.S) = struct
      module TM = TB.On (M)

      let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
        TM.map_m_xchg x ~f ~mu:(TM.map_m ~f)
    end
  end)

  module On_addresses :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Make_traversal (struct
    module Elt = Address
    module A =
      Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (Address)
    module C = Travesty.Traversable.Const (Constant) (Address)
    module L = Atomic_load.On_addresses
    module O = Travesty.Traversable.Const (Mem_order) (Address)
  end)

  module On_constants :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Constant.t =
  Make_traversal (struct
    module Elt = Constant
    module A = Travesty.Traversable.Const (Address) (Constant)
    module C =
      Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (Constant)
    module L = Travesty.Traversable.Const (Atomic_load) (Constant)
    module O = Travesty.Traversable.Const (Mem_order) (Constant)
  end)

  module On_mem_orders :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Mem_order.t = Make_traversal (struct
    module Elt = Mem_order
    module A = Travesty.Traversable.Const (Address) (Mem_order)
    module C = Travesty.Traversable.Const (Constant) (Mem_order)
    module L = Atomic_load.On_mem_orders
    module O =
      Travesty.Traversable.Fix_elt
        (Travesty_containers.Singleton)
        (Mem_order)
  end)
end

module Atomic :
  Expression_types.S_traversable
    with type t = Expression.t Atomic_expression.t = struct
  type t = Expression.t Atomic_expression.t

  module Make_traversal (Basic : Basic) = Travesty.Traversable.Make0 (struct
    type nonrec t = t

    module Elt = Basic.Elt
    module TB = Make_traversal_base (Basic)

    module On (M : Applicative.S) = struct
      module TM = TB.On (M)

      let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
        TM.map_m_ae x ~f ~mu:(TM.map_m ~f)
    end
  end)

  module On_addresses :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
  Make_traversal (struct
    module Elt = Address
    module A =
      Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (Address)
    module C = Travesty.Traversable.Const (Constant) (Address)
    module L = Atomic_load.On_addresses
    module O = Travesty.Traversable.Const (Mem_order) (Address)
  end)

  module On_constants :
    Travesty.Traversable_types.S0 with type t = t and type Elt.t = Constant.t =
  Make_traversal (struct
    module Elt = Constant
    module A = Travesty.Traversable.Const (Address) (Constant)
    module C =
      Travesty.Traversable.Fix_elt (Travesty_containers.Singleton) (Constant)
    module L = Travesty.Traversable.Const (Atomic_load) (Constant)
    module O = Travesty.Traversable.Const (Mem_order) (Constant)
  end)

  module On_mem_orders :
    Travesty.Traversable_types.S0
      with type t = t
       and type Elt.t = Mem_order.t = Make_traversal (struct
    module Elt = Mem_order
    module A = Travesty.Traversable.Const (Address) (Mem_order)
    module C = Travesty.Traversable.Const (Constant) (Mem_order)
    module L = Atomic_load.On_mem_orders
    module O =
      Travesty.Traversable.Fix_elt
        (Travesty_containers.Singleton)
        (Mem_order)
  end)
end

let depended_upon_idents :
    ( 'i
    , C4f_common.C_id.t
    , Expression.t
    , [< Accessor.many_getter] )
    Accessor.Simple.t =
  (* TODO(@MattWindsor91): if we ever get a more useful notion of dependency,
     replace this definition. *)
  [%accessor
    Accessor.(
      many_getter
        Many_getter.(
          On_addresses.fold ~init:empty ~f:(fun mg ad ->
              mg @ access ad.@(Address.variable_of))))]
