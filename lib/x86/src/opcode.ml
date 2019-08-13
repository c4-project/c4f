(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base
open Act_common
open Act_utils
module Tx = Travesty_base_exts

module Operand_spec = struct
  type single = Immediate | Memory | Register

  type t =
    | Zero
    | One of single list
    | Symmetric of (single * single) list
    | Src_dst of (single, single) Src_dst.t list
    | Or of t * t

  (* Operand spec for 'arithmetic'-style operands (where the source may be
     immediate). We also currently use it for MOV, treating OI as I, FD as
     RM, and TD as MR. *)
  let arith_operands =
    Src_dst
      Src_dst.
        [ {dst= Register; src= Immediate} (* I, MI *)
        ; {dst= Memory; src= Immediate} (* MI *)
        ; {dst= Register; src= Memory} (* RM *)
        ; {dst= Memory; src= Register} (* MR *)
        ; {dst= Register; src= Register}
          (* RM, MR *) ]
end

(* To add a new opcode:

   1. Add an entry for it into either 'Sizable.t' or 'Basic.t', in both here
   and the MLI file. It goes into 'Sizable.t' if, in AT&T syntax, it can
   have a size prefix.

   2. Add the string representation into the table below the enumerator.

   3. Add the appropriate abstract classifications for the opcodes and
   operands.

   5. Add legs to the pattern-matches in x86.Sanitiser. *)

(** [Sizable] enumerates X86 opcodes that may have an associated size
    directive (Intel) or suffix (AT&T). *)
module Sizable = struct
  type t =
    [ `Add
    | `Call (* Some compilers seem to emit CALLQ? *)
    | `Cmp
    | `Cmpxchg
    | `Mov
    | `Pop
    | `Push
    | `Ret
    | (* Some compilers seem to emit RETL (32-bit)/RETQ (64-bit); it's
         unclear if there's any semantic difference from RET. *)
      `Sub
    | `Xchg
    | `Xor ]
  [@@deriving sexp, equal, enumerate]

  (* Note: any SIZABLE opcodes not present in this table map to 'unknown
     operands'. *)
  let operand_table : ([> t], Operand_spec.t) List.Assoc.t =
    (* See the Intel reference manual for the source of these. *)
    Operand_spec.
      [ (`Add, arith_operands) (* TODO(@MattWindsor91): Call *)
      ; (`Cmp, arith_operands) (* TODO(@MattWindsor91): Cmpxchg *)
      ; (`Mov, arith_operands) (* see arith_operand_spec comments *)
      ; (`Pop, One [Memory; Register])
      ; (`Push, One [Memory; Register; Immediate])
      ; (`Ret, Or (Zero, One [Immediate]))
      ; (`Sub, arith_operands)
        (* Though the reference manual describes XCHG as having a source and
           destination operand, the two operands are symmetrical in the
           encoding, and both only accept destinations. *)
      ; (`Xchg, Symmetric [(Memory, Register); (Register, Register)])
      ; (`Xor, arith_operands) ]

  let get_operand_spec = List.Assoc.find operand_table ~equal

  include (
    String_table.Make (struct
      type nonrec t = t

      let equal = equal

      let table =
        [ (`Add, "add")
        ; (`Call, "call")
        ; (`Cmp, "cmp")
        ; (`Cmpxchg, "cmpxchg")
        ; (`Mov, "mov")
        ; (`Pop, "pop")
        ; (`Push, "push")
        ; (`Ret, "ret")
        ; (`Sub, "sub")
        ; (`Xchg, "xchg")
        ; (`Xor, "xor") ]
    end) :
      String_table.S with type t := t )

  include Act_abstract.Abstractable.Make (struct
    type nonrec t = t

    module Abs = Act_abstract.Instruction.Opcode
    open Abs

    let abstract = function
      | `Add ->
          Arith
      | `Call ->
          Call
      | `Cmp ->
          Compare
      | `Cmpxchg ->
          Rmw
      | `Mov ->
          Move
      | `Pop ->
          Stack
      | `Push ->
          Stack
      | `Ret ->
          Return
      | `Sub ->
          Arith
      | `Xchg ->
          Rmw
      | `Xor ->
          Logical
  end)
end

module Size = struct
  type t = Byte | Word | Long [@@deriving sexp, equal]

  module Suffix_table : String_table.S with type t := t =
  String_table.Make (struct
    type nonrec t = t

    let equal = equal

    let table = [(Byte, "b"); (Word, "w"); (Long, "l")]
  end)
end

module Sized = struct
  type t = Sizable.t * Size.t [@@deriving sexp, equal]

  include (
    String_table.Make (struct
      type nonrec t = t

      let equal = equal

      let table =
        List.map
          ~f:(fun ((op, ops), (sz, szs)) -> ((op, sz), ops ^ szs))
          (List.cartesian_product Sizable.table Size.Suffix_table.table)
    end) :
      String_table.S with type t := t )

  include Act_abstract.Abstractable.Make (struct
    type nonrec t = t

    module Abs = Act_abstract.Instruction.Opcode

    let abstract (s, _) = Sizable.abstract s
  end)
end

module Basic = struct
  type t = [Sizable.t | `Leave | `Mfence | `Nop]
  [@@deriving sexp, equal, enumerate]

  (* Note: any NON-SIZABLE opcodes not present in this table map to 'unknown
     operands'. *)
  let operand_table : ([> t], Operand_spec.t) List.Assoc.t =
    (* See the Intel reference manual for the source of these. *)
    Sizable.operand_table
    @ Operand_spec.[(`Leave, Zero); (`Mfence, Zero); (`Nop, Zero)]

  let get_operand_spec = List.Assoc.find operand_table ~equal

  include (
    String_table.Make (struct
      type nonrec t = t

      let equal = equal

      let table =
        (Sizable.table :> (t, string) List.Assoc.t)
        @ [(`Leave, "leave"); (`Mfence, "mfence"); (`Nop, "nop")]
    end) :
      String_table.S with type t := t )

  include Act_abstract.Abstractable.Make (struct
    type nonrec t = t

    module Abs = Act_abstract.Instruction.Opcode
    open Abs

    let abstract = function
      | #Sizable.t as s ->
          Sizable.abstract s
      | `Leave ->
          Call
      | `Mfence ->
          Fence
      | `Nop ->
          Nop
  end)
end

module Condition = struct
  type invertible =
    [ `Above
    | `AboveEqual
    | `Below
    | `BelowEqual
    | `Carry
    | `Equal
    | `Greater
    | `GreaterEqual
    | `Less
    | `LessEqual
    | `Overflow
    | `Parity
    | `Sign
    | `Zero ]
  [@@deriving sexp, equal, enumerate]

  (** Intermediate table used to build the main condition table. *)
  module Inv_table = String_table.Make (struct
    type t = invertible [@@deriving equal]

    let table =
      [ (`Above, "a")
      ; (`AboveEqual, "ae")
      ; (`Below, "b")
      ; (`BelowEqual, "be")
      ; (`Carry, "c")
      ; (`Equal, "e")
      ; (`Greater, "g")
      ; (`GreaterEqual, "ge")
      ; (`Less, "l")
      ; (`LessEqual, "le")
      ; (`Overflow, "o")
      ; (`Parity, "p")
      ; (`Sign, "s")
      ; (`Zero, "z") ]
  end)

  type t =
    [ invertible
    | `Not of invertible
    | `CXZero
    | `ECXZero
    | `ParityEven
    | `ParityOdd ]
  [@@deriving sexp, equal, enumerate]

  (** [build_inv_condition (ic, s)] builds, for an invertible condition C,
      string table entries for C and NC. *)
  let build_inv_condition (ic, s) = [((ic :> t), s); (`Not ic, "n" ^ s)]

  include (
    String_table.Make (struct
      type nonrec t = t

      let equal = equal

      let table =
        List.bind ~f:build_inv_condition Inv_table.table
        @ [ (`CXZero, "cxz")
          ; (`ECXZero, "ecxz")
          ; (`ParityEven, "pe")
          ; (`ParityOdd, "po") ]
    end) :
      String_table.S with type t := t )
end

module Jump = struct
  type t = [`Unconditional | `Conditional of Condition.t]
  [@@deriving sexp, equal, enumerate]

  include (
    String_table.Make (struct
      type nonrec t = t

      let equal = equal

      (* Jump instructions are always jC for some condition C, except jmp. *)
      let f (x, s) = (`Conditional x, "j" ^ s)

      let table = (`Unconditional, "jmp") :: List.map ~f Condition.table
    end) :
      String_table.S with type t := t )

  include Act_abstract.Abstractable.Make (struct
    type nonrec t = t

    module Abs = Act_abstract.Instruction.Opcode

    let abstract _ = Abs.Jump
  end)
end

type t =
  | Basic of Basic.t
  | Sized of Sized.t
  | Jump of Jump.t
  | Directive of string
  | Unknown of string
[@@deriving sexp, equal, variants]

include Act_abstract.Abstractable.Make (struct
  type nonrec t = t

  module Abs = Act_abstract.Instruction.Opcode

  let abstract = function
    | Basic b ->
        Basic.abstract b
    | Sized s ->
        Sized.abstract s
    | Jump j ->
        Jump.abstract j
    | Directive _ ->
        Other
    | Unknown _ ->
        Unknown
end)

let directive_of_string string = String.chop_prefix string ~prefix:"."

let of_string string =
  Tx.Option.first_some_of_thunks
    Option.
      [ (fun () -> string |> directive_of_string >>| directive)
      ; (fun () -> string |> Jump.of_string >>| jump)
      ; (fun () -> string |> Sized.of_string >>| sized)
      ; (fun () -> string |> Basic.of_string >>| basic) ]
  |> Option.value ~default:(Unknown string)

let call : t = Basic `Call

let jmp : t = Jump `Unconditional
