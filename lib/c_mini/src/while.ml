(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Kind = struct
  type t = While | Do_while [@@deriving sexp, compare, equal]
end

type ('meta, 'stm) t =
  {cond: Expression.t; body: ('meta, 'stm) Block.t; kind: Kind.t}
[@@deriving sexp, make, fields, compare, equal]

module Base_map (A : Applicative.S) = struct
  let bmap (type m1 s1 m2 s2) (while_loop : (m1, s1) t)
      ~(cond : Expression.t -> Expression.t A.t)
      ~(body : (m1, s1) Block.t -> (m2, s2) Block.t A.t)
      ~(kind : Kind.t -> Kind.t A.t) : (m2, s2) t A.t =
    let make cond body kind = make ~cond ~body ~kind in
    A.(
      return make <*> cond while_loop.cond <*> body while_loop.body
      <*> kind while_loop.kind)
end

(* TODO(@MattWindsor91): Travesty this and the one in If. *)
module Ident = struct
  type 'a t = 'a

  include Applicative.Make (struct
    type 'a t = 'a

    let apply f a = f a

    let return x = x

    let map = `Custom (fun a ~f -> f a)
  end)
end

module Bident = Base_map (Ident)

let map = Bident.bmap
