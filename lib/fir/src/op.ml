(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open struct
  module A = Accessor_base
end

let idem = A.construct Op_rule.idem
let unknown = A.construct Op_rule.unknown
let zero = A.construct Op_rule.zero
let always_unknown _ = unknown ()

module Unary = struct
  type t = L_not [@@deriving sexp, variants, compare, equal, quickcheck]
end

module Binary = struct
  module Rel = struct
    type t = Eq | Ne [@@deriving sexp, compare, equal, quickcheck]

    let zero_lhs : t -> Op_rule.t = always_unknown

    (* (0 == x) == ?; (x != 0) == ? *)

    let zero_rhs : t -> Op_rule.t = always_unknown

    (* (x == 0) == ?; (x != 0) == ? *)

    let refl : t -> Op_rule.t = always_unknown
  end

  module Arith = struct
    type t = Add | Sub [@@deriving sexp, compare, equal, quickcheck]

    let zero_lhs : t -> Op_rule.t = function
      | Add ->
          idem () (* 0+x == x *)
      | Sub ->
          unknown ()

    (* 0-x == ? *)

    let zero_rhs : t -> Op_rule.t = function
      | Add | Sub ->
          idem ()

    (* x+0 == x; x-0 == x *)

    let refl : t -> Op_rule.t = function
      | Sub ->
          zero ()
      | Add ->
          unknown ()
  end

  module Bitwise = struct
    type t = And | Or | Xor [@@deriving sexp, compare, equal, quickcheck]

    let zero_lhs : t -> Op_rule.t = function
      | Or | Xor ->
          idem () (* x|0 == x; x^0 == x *)
      | And ->
          zero ()

    (* x&0 == 0 *)

    (* All bitwise operators are commutative. *)
    let zero_rhs : t -> Op_rule.t = zero_lhs

    let refl : t -> Op_rule.t = function
      | Xor ->
          zero ()
      | And | Or ->
          idem ()
  end

  module Logical = struct
    type t = And | Or [@@deriving sexp, compare, equal, quickcheck]

    let zero_lhs : t -> Op_rule.t = always_unknown

    let zero_rhs : t -> Op_rule.t = zero_lhs

    let refl : t -> Op_rule.t = always_unknown
  end

  type t =
    | Rel of Rel.t
    | Arith of Arith.t
    | Bitwise of Bitwise.t
    | Logical of Logical.t
  [@@deriving sexp, variants, compare, equal, quickcheck]

  let eq : t = Rel Eq

  let ne : t = Rel Ne

  let add : t = Arith Add

  let sub : t = Arith Sub

  let l_and : t = Logical And

  let l_or : t = Logical Or

  let b_and : t = Bitwise And

  let b_or : t = Bitwise Or

  let b_xor : t = Bitwise Xor

  let zero_lhs : t -> Op_rule.t = function
    | Rel o ->
        Rel.zero_lhs o
    | Arith o ->
        Arith.zero_lhs o
    | Bitwise o ->
        Bitwise.zero_lhs o
    | Logical o ->
        Logical.zero_lhs o

  let zero_rhs : t -> Op_rule.t = function
    | Rel o ->
        Rel.zero_rhs o
    | Arith o ->
        Arith.zero_rhs o
    | Bitwise o ->
        Bitwise.zero_rhs o
    | Logical o ->
        Logical.zero_rhs o

  let refl : t -> Op_rule.t = function
    | Rel o ->
        Rel.refl o
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

  let zero_lhs : t -> Op_rule.t =
    Fn.compose Binary.zero_lhs to_bop

  let zero_rhs : t -> Op_rule.t =
    Fn.compose Binary.zero_rhs to_bop

  let refl : t -> Op_rule.t = Fn.compose Binary.refl to_bop

  module Gen_idem_zero_rhs = struct
    include With_qc

    (* add, sub, OR, XOR, etc. *)

    let quickcheck_generator : t Base_quickcheck.Generator.t =
      Base_quickcheck.Generator.filter quickcheck_generator
        ~f:(Fn.compose Op_rule.is_idem zero_rhs)
  end

  module Gen_idem_refl = struct
    include With_qc

    (* AND and OR *)

    let quickcheck_generator : t Base_quickcheck.Generator.t =
      Base_quickcheck.Generator.filter quickcheck_generator
        ~f:(Fn.compose Op_rule.is_idem refl)
  end
end
