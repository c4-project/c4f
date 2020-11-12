(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

module Unary = struct
  type t = L_not
  [@@deriving enumerate, sexp, variants, compare, equal, quickcheck]
end

module Binary = struct
  module Rel = struct
    type t = Eq | Ne | Gt | Ge | Le | Lt
    [@@deriving enumerate, sexp, compare, equal, quickcheck]

    let rules : t -> Op_rule.t list =
      (* If we ever get unsigned integers, or some way of tracking what the
         maximum and minimum integers are in constant form, we can add a few
         more rules for Gt/Ge/Le/Lt. *)
      Op_rule.(
        function
        | Eq | Le | Ge ->
            [Refl @-> Const Constant.truth (* x == x -> true, etc. *)]
        | Ne | Lt | Gt ->
            [Refl @-> Const Constant.falsehood (* x != x -> false, etc. *)])
  end

  module Arith = struct
    type t = Add | Sub
    [@@deriving enumerate, sexp, compare, equal, quickcheck]

    let rules : t -> Op_rule.t list =
      Op_rule.(
        function
        | Add ->
            [ In.zero Left @-> Idem (* 0 + x -> x *)
            ; In.zero Right @-> Idem
              (* x + 0 -> x *) ]
        | Sub ->
            [ In.zero Right @-> Idem (* x - 0 -> x *)
            ; Refl @-> Out.zero
              (* x - x -> 0 *) ])
  end

  module Bitwise = struct
    type t = And | Or | Xor
    [@@deriving enumerate, sexp, compare, equal, quickcheck]

    (** Rules that are probably only valid in twos'-complement systems. *)
    let twos_complement_rules : Op_rule.t list =
      Op_rule.
        [ In.minus_one Left @-> Idem (* x&-1 -> x*)
        ; In.minus_one Right @-> Idem
          (* -1&x -> x*) ]

    let rules : t -> Op_rule.t list =
      Op_rule.(
        function
        | And ->
            [ In.zero Left @-> Out.zero (* 0&x -> 0 *)
            ; In.zero Right @-> Out.zero (* x&0 -> 0 *)
            ; Refl @-> Idem
              (* x&x -> x *) ]
            @ twos_complement_rules
        | Or ->
            [ In.zero Left @-> Idem (* 0|x -> x *)
            ; In.zero Right @-> Idem (* x|0 -> x *)
            ; Refl @-> Idem
              (* x|x -> x *) ]
        | Xor ->
            [ In.zero Left @-> Idem (* 0^x -> x *)
            ; In.zero Right @-> Idem (* x^0 -> x *)
            ; Refl @-> Out.zero
              (* x^x -> 0 *) ])
  end

  module Logical = struct
    type t = And | Or
    [@@deriving enumerate, sexp, compare, equal, quickcheck]

    let rules : t -> Op_rule.t list =
      Op_rule.(
        function
        | And ->
            [ In.false_ Left @-> Out.false_ (* false&&x -> false *)
            ; In.false_ Right @-> Out.false_ (* x&&false -> false *)
            ; In.true_ Left @-> Idem (* true&&x -> x *)
            ; In.true_ Right @-> Idem (* x&&true -> x *)
            ; Refl @-> Idem
              (* x&&x -> x *) ]
        | Or ->
            [ In.false_ Left @-> Idem (* false||x -> x *)
            ; In.false_ Right @-> Idem (* x||false -> x *)
            ; In.true_ Left @-> Out.true_ (* true||x -> true *)
            ; In.true_ Right @-> Out.true_ (* x||true -> true *)
            ; Refl @-> Idem
              (* x&&x -> x *) ])
  end

  type t =
    | Rel of Rel.t
    | Arith of Arith.t
    | Bitwise of Bitwise.t
    | Logical of Logical.t
  [@@deriving enumerate, sexp, variants, compare, equal, quickcheck]

  let eq : t = Rel Eq

  let ne : t = Rel Ne

  let lt : t = Rel Lt

  let le : t = Rel Le

  let ge : t = Rel Ge

  let gt : t = Rel Gt

  let add : t = Arith Add

  let sub : t = Arith Sub

  let l_and : t = Logical And

  let l_or : t = Logical Or

  let b_and : t = Bitwise And

  let b_or : t = Bitwise Or

  let b_xor : t = Bitwise Xor

  let rules : t -> Op_rule.t list = function
    | Rel o ->
        Rel.rules o
    | Arith o ->
        Arith.rules o
    | Bitwise o ->
        Bitwise.rules o
    | Logical o ->
        Logical.rules o
end

module Fetch = struct
  module With_qc = struct
    module M = struct
      type t = Add | Sub | And | Or | Xor [@@deriving enum, enumerate]

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

  let rules : t -> Op_rule.t list = Fn.compose Binary.rules to_bop

  module Gen_idem_zero_rhs = struct
    include With_qc

    (* add, sub, OR, XOR, etc. *)

    let quickcheck_generator : t Base_quickcheck.Generator.t =
      Base_quickcheck.Generator.filter quickcheck_generator
        ~f:
          (Fn.compose
             Op_rule.(has_in_out_matching (In.zero' Right) Idem)
             rules)
  end

  module Gen_idem_refl = struct
    include With_qc

    (* AND and OR *)

    let quickcheck_generator : t Base_quickcheck.Generator.t =
      Base_quickcheck.Generator.filter quickcheck_generator
        ~f:(Fn.compose Op_rule.(has_in_out_matching In.refl Idem) rules)
  end
end
