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

    let zero_lhs_unit : t -> bool = function Add -> true | Sub -> false

    let zero_rhs_unit : t -> bool = function Add | Sub -> true

    let refl_zero : t -> bool = function Sub -> true | Add -> false
  end

  module Bitwise = struct
    type t = And | Or | Xor [@@deriving sexp, compare, equal, quickcheck]

    let zero_lhs_unit : t -> bool = function
      | Or | Xor ->
          true
      | And ->
          false

    (* All bitwise operators are commutative. *)
    let zero_rhs_unit = zero_lhs_unit

    let refl_zero : t -> bool = function Xor -> true | And | Or -> false
  end

  module Logical = struct
    type t = And | Or [@@deriving sexp, compare, equal, quickcheck]

    (* TODO(@MattWindsor91): can we rely on zero-unit for any logical
       operators? *)
    let zero_lhs_unit : t -> bool = Fn.const false

    (* All logical operators are commutative. *)
    let zero_rhs_unit = zero_lhs_unit

    let refl_zero : t -> bool = Fn.const false
  end

  type t =
    | Eq
    | Arith of Arith.t
    | Bitwise of Bitwise.t
    | Logical of Logical.t
  [@@deriving sexp, variants, compare, equal, quickcheck]

  let add : t = Arith Add

  let sub : t = Arith Sub

  let l_and : t = Logical And

  let l_or : t = Logical Or

  let b_and : t = Bitwise And

  let b_or : t = Bitwise Or

  let b_xor : t = Bitwise Xor

  let zero_lhs_unit : t -> bool = function
    | Eq ->
        false
    | Arith o ->
        Arith.zero_lhs_unit o
    | Bitwise o ->
        Bitwise.zero_lhs_unit o
    | Logical o ->
        Logical.zero_lhs_unit o

  let zero_rhs_unit : t -> bool = function
    | Eq ->
        false
    | Arith o ->
        Arith.zero_rhs_unit o
    | Bitwise o ->
        Bitwise.zero_rhs_unit o
    | Logical o ->
        Logical.zero_rhs_unit o

  let refl_zero : t -> bool = function
    | Eq ->
        false
    | Arith o ->
        Arith.refl_zero o
    | Bitwise o ->
        Bitwise.refl_zero o
    | Logical o ->
        Logical.refl_zero o
end

module Fetch = struct
  module M = struct
    type t = Add | Sub | And | Or | Xor [@@deriving enum]

    let table =
      [(Add, "add"); (Sub, "sub"); (Or, "or"); (Xor, "xor"); (And, "and")]
  end

  include M
  include Act_utils.Enum.Extend_table (M)

  let to_bop : t -> Binary.t = function
    | Add ->
        Binary.add
    | Sub ->
        Binary.sub
    | Or ->
        Binary.b_or
    | Xor ->
        Binary.b_xor
    | And ->
        Binary.b_and

  let zero_lhs_unit : t -> bool = Fn.compose Binary.zero_lhs_unit to_bop

  let zero_rhs_unit : t -> bool = Fn.compose Binary.zero_rhs_unit to_bop

  let refl_zero : t -> bool = Fn.compose Binary.refl_zero to_bop
end
