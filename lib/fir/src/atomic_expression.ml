(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module P = struct
  type 'e t =
    | Cmpxchg of 'e Atomic_cmpxchg.t
    | Fetch of 'e Atomic_fetch.t
    | Load of Atomic_load.t
  [@@deriving sexp, variants, compare, equal, quickcheck]
end

include P

let reduce (ae : 'e t) ~(cmpxchg : 'e Atomic_cmpxchg.t -> 'a)
    ~(fetch : 'e Atomic_fetch.t -> 'a) ~(load : Atomic_load.t -> 'a) : 'a =
  match ae with
  | Cmpxchg c ->
      cmpxchg c
  | Fetch f ->
      fetch f
  | Load l ->
      load l

module Base_map (Ap : Applicative.S) = struct
  let bmap (x : 'a t)
      ~(cmpxchg : 'a Atomic_cmpxchg.t -> 'b Atomic_cmpxchg.t Ap.t)
      ~(fetch : 'a Atomic_fetch.t -> 'b Atomic_fetch.t Ap.t)
      ~(load : Atomic_load.t -> Atomic_load.t Ap.t) : 'b t Ap.t =
    Travesty_base_exts.Fn.Compose_syntax.(
      reduce x
        ~cmpxchg:(cmpxchg >> Ap.map ~f:P.cmpxchg)
        ~fetch:(fetch >> Ap.map ~f:P.fetch)
        ~load:(load >> Ap.map ~f:P.load))
end

module On_expressions : Travesty.Traversable_types.S1 with type 'e t = 'e t =
Travesty.Traversable.Make1 (struct
  type nonrec 'e t = 'e t

  module On (M : Applicative.S) = struct
    module B = Base_map (M)
    module Ac = Atomic_cmpxchg.On_expressions.On (M)
    module Af = Atomic_fetch.On_expressions.On (M)

    let map_m (x : 'a t) ~(f : 'a -> 'b M.t) : 'b t M.t =
      B.bmap x ~cmpxchg:(Ac.map_m ~f) ~fetch:(Af.map_m ~f) ~load:M.return
  end
end)

module Type_check (E : Env_types.S) :
  Types.S_type_checker with type t := Type.t t = struct
  module Ac = Atomic_cmpxchg.Type_check (E)
  module Af = Atomic_fetch.Type_check (E)
  module Al = Atomic_load.Type_check (E)

  let type_of : Type.t t -> Type.t Or_error.t =
    reduce ~cmpxchg:Ac.type_of ~fetch:Af.type_of ~load:Al.type_of
end
