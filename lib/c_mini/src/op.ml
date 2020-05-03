(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Algebra = struct
  let is_idem : [> `Idem] option -> bool = function
    | Some `Idem ->
        true
    | _ ->
        false

  let is_zero : [> `Zero] option -> bool = function
    | Some `Zero ->
        true
    | _ ->
        false
end

module Unary = struct
  type t = L_not [@@deriving sexp, variants, compare, equal, quickcheck]
end

module Binary = struct
  module Arith = struct
    type t = Add | Sub [@@deriving sexp, compare, equal, quickcheck]

    let zero_lhs : t -> [`Idem | `Zero] option = function
      | Add ->
          Some `Idem (* 0+x == x *)
      | Sub ->
          None

    (* 0-x == ? *)

    let zero_rhs : t -> [`Idem | `Zero] option = function
      | Add | Sub ->
          Some `Idem

    (* x+0 == x; x-0 == x *)

    let refl : t -> [`Idem | `Zero] option = function
      | Sub ->
          Some `Zero
      | Add ->
          None
  end

  module Bitwise = struct
    type t = And | Or | Xor [@@deriving sexp, compare, equal, quickcheck]

    let zero_lhs : t -> [`Idem | `Zero] option = function
      | Or | Xor ->
          Some `Idem (* x|0 == x; x^0 == x *)
      | And ->
          Some `Zero

    (* x&0 == 0 *)

    (* All bitwise operators are commutative. *)
    let zero_rhs = zero_lhs

    let refl : t -> [`Idem | `Zero] option = function
      | Xor ->
          Some `Zero
      | And | Or ->
          Some `Idem
  end

  module Logical = struct
    type t = And | Or [@@deriving sexp, compare, equal, quickcheck]

    (* TODO(@MattWindsor91): can we rely on properties for any logical
       operators? *)
    let zero_lhs : t -> [`Idem | `Zero] option = Fn.const None

    let zero_rhs = zero_lhs

    let refl : t -> [`Idem | `Zero] option = Fn.const None
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

  let zero_lhs : t -> [`Idem | `Zero] option = function
    | Eq ->
        None
    | Arith o ->
        Arith.zero_lhs o
    | Bitwise o ->
        Bitwise.zero_lhs o
    | Logical o ->
        Logical.zero_lhs o

  let zero_rhs : t -> [`Idem | `Zero] option = function
    | Eq ->
        None
    | Arith o ->
        Arith.zero_rhs o
    | Bitwise o ->
        Bitwise.zero_rhs o
    | Logical o ->
        Logical.zero_rhs o

  let refl : t -> [`Idem | `Zero] option = function
    | Eq ->
        None
    | Arith o ->
        Arith.refl o
    | Bitwise o ->
        Bitwise.refl o
    | Logical o ->
        Logical.refl o
end

module Fetch = struct
  module With_qc = struct
    module M = struct
      type t = Add | Sub | And | Or | Xor [@@deriving enum]

      let table =
        [(Add, "add"); (Sub, "sub"); (Or, "or"); (Xor, "xor"); (And, "and")]
    end

    include M
    include Act_utils.Enum.Extend_table (M)
  end

  include With_qc

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

  let zero_lhs : t -> [`Idem | `Zero] option =
    Fn.compose Binary.zero_lhs to_bop

  let zero_rhs : t -> [`Idem | `Zero] option =
    Fn.compose Binary.zero_rhs to_bop

  let refl : t -> [`Idem | `Zero] option = Fn.compose Binary.refl to_bop

  module Gen_idem_zero_rhs = struct
    include With_qc

    (* add, sub, OR, XOR, etc. *)

    let quickcheck_generator : t Base_quickcheck.Generator.t =
      Base_quickcheck.Generator.filter quickcheck_generator
        ~f:(Fn.compose Algebra.is_idem zero_rhs)
  end

  module Gen_idem_refl = struct
    include With_qc

    (* AND and OR *)

    let quickcheck_generator : t Base_quickcheck.Generator.t =
      Base_quickcheck.Generator.filter quickcheck_generator
        ~f:(Fn.compose Algebra.is_idem refl)
  end
end
