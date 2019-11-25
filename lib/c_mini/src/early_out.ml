(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Kind = struct
  type t = Break | Return [@@deriving sexp, equal]
end

type 'meta t = {kind: Kind.t; meta: 'meta} [@@deriving fields, sexp, equal]

let break (meta : 'meta) : 'meta t = {kind= Break; meta}

let return (meta : 'meta) : 'meta t = {kind= Return; meta}

module On_meta : Travesty.Traversable_types.S1 with type 'meta t := 'meta t =
Travesty.Traversable.Make1 (struct
  type nonrec 'meta t = 'meta t

  module On_monad (M : Monad.S) = struct
    let map_m (x : 'm1 t) ~(f : 'm1 -> 'm2 M.t) : 'm2 t M.t =
      M.Let_syntax.(
        let%map meta = f x.meta in
        {meta; kind= x.kind})
  end
end)
