(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Import

type ('m, 'v) t = {meta: 'm; value: 'v}
[@@deriving sexp, compare, equal, accessors]

let make (value : 'v) ~(meta : 'm) : ('m, 'v) t = {meta; value}

let no_meta :
    type i v1 v2.
    ( i -> v1 -> v2
    , i -> (unit, v1) t -> (unit, v2) t
    , [< isomorphism] )
    Accessor.t =
  [%accessor
    Accessor.isomorphism ~get:(Accessor.get value) ~construct:(fun value ->
        {meta= (); value})]

module BT :
  Travesty.Bi_traversable_types.S2 with type ('m, 'v) t := ('m, 'v) t =
Travesty.Bi_traversable.Make2 (struct
  type nonrec ('m, 'v) t = ('m, 'v) t

  module On_monad (M : Monad.S) = struct
    module A = Applicative.Of_monad (M)

    let bi_map_m (x : ('m1, 'v1) t) ~(left : 'm1 -> 'm2 M.t)
        ~(right : 'v1 -> 'v2 M.t) : ('m2, 'v2) t M.t =
      A.(
        return (fun meta value -> make ~meta value)
        <*> left x.meta <*> right x.value)
  end
end)

include BT
