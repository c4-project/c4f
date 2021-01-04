(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

module Liveness = struct
  type t = Dead | Once | Live [@@deriving accessors, sexp, compare, equal]

  let is_dead (x : t) : bool = not (Accessor.is_empty dead x)
end

module Restriction = struct
  module M = struct
    type t = Once_only [@@deriving accessors, sexp, compare, equal]
  end

  include M
  include Comparable.Make (M)
end

module Gen = struct
  type t =
    { liveness: Liveness.t [@default Liveness.Live]
    ; restrictions: Set.M(Restriction).t
          [@default Set.empty (module Restriction)] }
  [@@deriving accessors, sexp, make, compare, equal]
end

type t = Existing | Generated of Gen.t
[@@deriving accessors, sexp, compare, equal]

let gen_normal : t = Generated (Gen.make ())

let gen_dead : t = Generated (Gen.make ~liveness:Dead ())

let gen_once : t = Generated (Gen.make ~liveness:Once ())

let liveness (m : t) : Liveness.t =
  Option.value m.@?(generated @> Gen.liveness) ~default:Live

let has_restriction (r : Restriction.t) : t -> bool = function
  | Existing ->
      true
  | Generated g ->
      not Accessor.(is_empty (Gen.restrictions @> Set.found r) g)
