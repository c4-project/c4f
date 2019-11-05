(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t =
  { obj: Address.t
  ; expected: Address.t
  ; desired: Expression.t
  ; succ: Mem_order.t
  ; fail: Mem_order.t }
[@@deriving sexp, fields, make, equal]

module Base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let bmap (cmpxchg : t) ~(obj : Address.t F.traversal)
      ~(expected : Address.t F.traversal)
      ~(desired : Expression.t F.traversal) ~(succ : Mem_order.t F.traversal)
      ~(fail : Mem_order.t F.traversal) : t M.t =
    Fields.fold ~init:(M.return cmpxchg) ~obj:(F.proc_field obj)
      ~expected:(F.proc_field expected) ~desired:(F.proc_field desired)
      ~succ:(F.proc_field succ) ~fail:(F.proc_field fail)
end

module On_lvalues :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Lvalue.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Lvalue

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module E = Expression.On_lvalues.On_monad (M)
    module A = Address.On_lvalues.On_monad (M)

    let map_m x ~f =
      B.bmap x ~obj:(A.map_m ~f) ~expected:(A.map_m ~f) ~desired:(E.map_m ~f)
        ~succ:M.return ~fail:M.return
  end
end)

module On_addresses :
  Travesty.Traversable_types.S0 with type t = t and type Elt.t = Address.t =
Travesty.Traversable.Make0 (struct
  type nonrec t = t

  module Elt = Address

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module E = Expression.On_addresses.On_monad (M)

    let map_m x ~f =
      B.bmap x ~obj:f ~expected:f ~desired:(E.map_m ~f) ~succ:M.return
        ~fail:M.return
  end
end)
