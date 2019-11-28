(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

(* This module wrapper exists to let us refer to the generated variant
   constructors in Base_map. *)
module P = struct
  type 'meta t =
    | Assign of Assign.t
    | Atomic_store of Atomic_store.t
    | Atomic_cmpxchg of Atomic_cmpxchg.t
    | Early_out of 'meta Early_out.t
    | Label of 'meta Label.t
    | Goto of 'meta Label.t
    | Nop of 'meta
  [@@deriving variants, sexp, equal]
end

include P

let break (meta : 'meta) : 'meta t = Early_out (Early_out.break meta)

let return (meta : 'meta) : 'meta t = Early_out (Early_out.return meta)

let reduce (type meta result) (x : meta t)
    ~(assign : (* meta *) Assign.t -> result)
    ~(atomic_store : (* meta *) Atomic_store.t -> result)
    ~(atomic_cmpxchg : (* meta *) Atomic_cmpxchg.t -> result)
    ~(early_out : meta Early_out.t -> result)
    ~(label : meta Label.t -> result) ~(goto : meta Label.t -> result)
    ~(nop : meta -> result) : result =
  Variants.map x ~assign:(Fn.const assign)
    ~atomic_store:(Fn.const atomic_store)
    ~atomic_cmpxchg:(Fn.const atomic_cmpxchg) ~early_out:(Fn.const early_out)
    ~label:(Fn.const label) ~goto:(Fn.const goto) ~nop:(Fn.const nop)

module Base_map (M : Monad.S) = struct
  module F = Travesty.Traversable.Helpers (M)

  let bmap (type m1 m2) (x : m1 t) ~assign ~atomic_store ~atomic_cmpxchg
      ~(early_out : m1 Early_out.t -> m2 Early_out.t M.t)
      ~(label : m1 Label.t -> m2 Label.t M.t)
      ~(goto : m1 Label.t -> m2 Label.t M.t) ~(nop : m1 -> m2 M.t) : m2 t M.t
      =
    Travesty_base_exts.Fn.Compose_syntax.(
      reduce x
        ~assign:(assign >> M.map ~f:P.assign)
        ~atomic_store:(atomic_store >> M.map ~f:P.atomic_store)
        ~atomic_cmpxchg:(atomic_cmpxchg >> M.map ~f:P.atomic_cmpxchg)
        ~early_out:(early_out >> M.map ~f:P.early_out)
        ~label:(label >> M.map ~f:P.label)
        ~goto:(goto >> M.map ~f:P.goto)
        ~nop:(nop >> M.map ~f:P.nop))
end

module On_meta : Travesty.Traversable_types.S1 with type 'meta t := 'meta t =
Travesty.Traversable.Make1 (struct
  type nonrec 'meta t = 'meta t

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module EO = Early_out.On_meta.On_monad (M)
    module LO = Label.On_meta.On_monad (M)

    let map_m (x : 'm1 t) ~(f : 'm1 -> 'm2 M.t) : 'm2 t M.t =
      B.bmap x ~assign:M.return ~atomic_store:M.return
        ~atomic_cmpxchg:M.return ~early_out:(EO.map_m ~f)
        ~label:(LO.map_m ~f) ~goto:(LO.map_m ~f) ~nop:f
  end
end)

module With_meta (Meta : T) = struct
  (** Does the legwork of implementing a particular type of traversal over
      statements. *)
  module Make_traversal (Basic : sig
    module Elt : Equal.S

    module A :
      Travesty.Traversable_types.S0
        with type t := Assign.t
         and module Elt = Elt

    module C :
      Travesty.Traversable_types.S0
        with type t := Atomic_cmpxchg.t
         and module Elt = Elt

    module S :
      Travesty.Traversable_types.S0
        with type t := Atomic_store.t
         and module Elt = Elt
  end) =
  Travesty.Traversable.Make0 (struct
    type nonrec t = Meta.t t

    module Elt = Basic.Elt

    module On_monad (M : Monad.S) = struct
      module SBase = Base_map (M)
      module AM = Basic.A.On_monad (M)
      module CM = Basic.C.On_monad (M)
      module SM = Basic.S.On_monad (M)

      let map_m x ~f =
        SBase.bmap x ~assign:(AM.map_m ~f) ~atomic_store:(SM.map_m ~f)
          ~atomic_cmpxchg:(CM.map_m ~f) ~early_out:M.return ~label:M.return
          ~goto:M.return ~nop:M.return
    end
  end)

  module On_lvalues :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Lvalue.t = Make_traversal (struct
    module Elt = Lvalue
    module A = Assign.On_lvalues
    module C = Atomic_cmpxchg.On_lvalues
    module S = Atomic_store.On_lvalues
  end)

  module On_addresses :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Address.t = Make_traversal (struct
    module Elt = Address
    module A = Assign.On_addresses
    module C = Atomic_cmpxchg.On_addresses
    module S = Atomic_store.On_addresses
  end)

  module On_identifiers :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Act_common.C_id.t =
    Travesty.Traversable.Chain0 (On_lvalues) (Lvalue.On_identifiers)
end
