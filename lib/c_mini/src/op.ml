(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Unary = struct
  type t = L_not [@@deriving sexp, variants, compare, equal, quickcheck]
end

module Binary = struct
  module Arith = struct
    type t = Add | Sub [@@deriving sexp, compare, equal, quickcheck]
  end

  module Logical = struct
    type t = And | Or [@@deriving sexp, compare, equal, quickcheck]
  end

  type t = Eq | Arith of Arith.t | Logical of Logical.t
  [@@deriving sexp, variants, compare, equal, quickcheck]

  let add : t = Arith Add

  let sub : t = Arith Sub

  let l_and : t = Logical And

  let l_or : t = Logical Or
end

module Fetch = struct
  module M = struct
    type t = Add | Sub [@@deriving enum]

    (* TODO(@MattWindsor91): or, xor, and *)

    let table = [(Add, "add"); (Sub, "sub")]
  end

  include M
  include Act_utils.Enum.Extend_table (M)

  let to_bop : t -> Binary.t = function
    | Add ->
        Binary.add
    | Sub ->
        Binary.sub
end
