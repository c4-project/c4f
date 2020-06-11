(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type ('meta, 'stm) t =
  { cond: Expression.t
  ; t_branch: ('meta, 'stm) Block.t
  ; f_branch: ('meta, 'stm) Block.t }
[@@deriving sexp, make, fields, compare, equal]

module Base_map (A : Applicative.S) = struct
  let bmap (type m1 s1 m2 s2) (if_stm : (m1, s1) t)
      ~(cond : Expression.t -> Expression.t A.t)
      ~(t_branch : (m1, s1) Block.t -> (m2, s2) Block.t A.t)
      ~(f_branch : (m1, s1) Block.t -> (m2, s2) Block.t A.t) : (m2, s2) t A.t
      =
    let make cond t_branch f_branch = make ~cond ~t_branch ~f_branch in
    A.(
      return make <*> cond if_stm.cond <*> t_branch if_stm.t_branch
      <*> f_branch if_stm.f_branch)
end

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
