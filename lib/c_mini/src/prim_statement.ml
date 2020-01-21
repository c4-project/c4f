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
    | Atomic of 'meta * Atomic_statement.t
    | Early_out of 'meta Early_out.t
    | Label of 'meta Label.t
    | Goto of 'meta Label.t
    | Nop of 'meta
    | Procedure_call of 'meta Call.t
  [@@deriving variants, sexp, compare, equal]
end

include P

let atomic_cmpxchg (meta : 'meta) (a : Atomic_cmpxchg.t) : 'meta t =
  atomic meta (Atomic_statement.atomic_cmpxchg a)

let atomic_fence (meta : 'meta) (a : Atomic_fence.t) : 'meta t =
  atomic meta (Atomic_statement.atomic_fence a)

let atomic_store (meta : 'meta) (a : Atomic_store.t) : 'meta t =
  atomic meta (Atomic_statement.atomic_store a)

let break (meta : 'meta) : 'meta t = Early_out (Early_out.break meta)

let continue (meta : 'meta) : 'meta t = Early_out (Early_out.continue meta)

let return (meta : 'meta) : 'meta t = Early_out (Early_out.return meta)

let reduce (type meta result) (x : meta t) ~(assign : Assign.t -> result)
    ~(atomic : meta * Atomic_statement.t -> result)
    ~(early_out : meta Early_out.t -> result)
    ~(label : meta Label.t -> result) ~(goto : meta Label.t -> result)
    ~(nop : meta -> result) ~(procedure_call : meta Call.t -> result) :
    result =
  Variants.map x ~assign:(Fn.const assign)
    ~atomic:(Fn.const (fun m a -> atomic (m, a)))
    ~early_out:(Fn.const early_out) ~label:(Fn.const label)
    ~goto:(Fn.const goto) ~nop:(Fn.const nop)
    ~procedure_call:(Fn.const procedure_call)

module Base_map (M : Monad.S) = struct
  let bmap (type m1 m2) (x : m1 t) ~(assign : Assign.t -> Assign.t M.t)
      ~(atomic : m1 * Atomic_statement.t -> (m2 * Atomic_statement.t) M.t)
      ~(early_out : m1 Early_out.t -> m2 Early_out.t M.t)
      ~(label : m1 Label.t -> m2 Label.t M.t)
      ~(goto : m1 Label.t -> m2 Label.t M.t) ~(nop : m1 -> m2 M.t)
      ~(procedure_call : m1 Call.t -> m2 Call.t M.t) : m2 t M.t =
    Travesty_base_exts.Fn.Compose_syntax.(
      reduce x
        ~assign:(assign >> M.map ~f:P.assign)
        ~atomic:(atomic >> M.map ~f:(fun (m, a) -> P.atomic m a))
        ~early_out:(early_out >> M.map ~f:P.early_out)
        ~label:(label >> M.map ~f:P.label)
        ~goto:(goto >> M.map ~f:P.goto)
        ~nop:(nop >> M.map ~f:P.nop)
        ~procedure_call:(procedure_call >> M.map ~f:P.procedure_call))
end

module On_meta : Travesty.Traversable_types.S1 with type 'meta t := 'meta t =
Travesty.Traversable.Make1 (struct
  type nonrec 'meta t = 'meta t

  module On_monad (M : Monad.S) = struct
    module B = Base_map (M)
    module EO = Early_out.On_meta.On_monad (M)
    module LO = Label.On_meta.On_monad (M)
    module CO = Call.On_meta.On_monad (M)

    let map_m (x : 'm1 t) ~(f : 'm1 -> 'm2 M.t) : 'm2 t M.t =
      B.bmap x ~assign:M.return
        ~atomic:(fun (m, x) -> M.(m |> f >>| fun m' -> (m', x)))
        ~early_out:(EO.map_m ~f) ~label:(LO.map_m ~f) ~goto:(LO.map_m ~f)
        ~nop:f ~procedure_call:(CO.map_m ~f)
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
        with type t := Meta.t Call.t
         and module Elt = Elt

    module T :
      Travesty.Traversable_types.S0
        with type t := Atomic_statement.t
         and module Elt = Elt
  end) =
  Travesty.Traversable.Make0 (struct
    type nonrec t = Meta.t t

    module Elt = Basic.Elt

    module On_monad (M : Monad.S) = struct
      module SBase = Base_map (M)
      module AM = Basic.A.On_monad (M)
      module CM = Basic.C.On_monad (M)
      module TM = Basic.T.On_monad (M)

      let map_atomic ((m, a) : Meta.t * Atomic_statement.t)
          ~(f : Elt.t -> Elt.t M.t) : (Meta.t * Atomic_statement.t) M.t =
        M.(a |> TM.map_m ~f >>| fun a' -> (m, a'))

      let map_m (x : t) ~(f : Elt.t -> Elt.t M.t) : t M.t =
        SBase.bmap x ~assign:(AM.map_m ~f) ~atomic:(map_atomic ~f)
          ~early_out:M.return ~label:M.return ~goto:M.return ~nop:M.return
          ~procedure_call:(CM.map_m ~f)
    end
  end)

  module Call = Call.With_meta (Meta)

  module On_lvalues :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Lvalue.t = Make_traversal (struct
    module Elt = Lvalue
    module A = Assign.On_lvalues
    module C = Call.On_lvalues
    module T = Atomic_statement.On_lvalues
  end)

  module On_addresses :
    Travesty.Traversable_types.S0
      with type t = Meta.t t
       and type Elt.t = Address.t = Make_traversal (struct
    module Elt = Address
    module A = Assign.On_addresses
    module C = Call.On_addresses
    module T = Atomic_statement.On_addresses
  end)
end
