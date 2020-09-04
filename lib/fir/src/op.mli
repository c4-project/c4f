(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: operators *)

(** {1 Expression operators} *)

(** {2 Unary operators} *)

module Unary : sig
  (** Enumeration of unary operators. *)
  type t = L_not [@@deriving sexp, compare, equal, quickcheck]

  val l_not : t
  (** [l_not] is the logical negation operator. *)
end

(** {2 Binary operators} *)

module Binary : sig
  (** {3 Arithmetic binary operators} *)

  module Arith : sig
    (** Enumeration of arithmetic binary operators. *)
    type t =
      | Add  (** An addition operator. *)
      | Sub  (** A subtraction operator. *)
    [@@deriving sexp, compare, equal, quickcheck]

    include Op_types.S_binary with type t := t
  end

  (** {3 Bitwise binary operators} *)

  module Bitwise : sig
    (** Enumeration of bitwise binary operators. *)
    type t =
      | And  (** A bitwise AND operator. *)
      | Or  (** A bitwise OR operator. *)
      | Xor  (** A bitwise XOR operator. *)
    [@@deriving sexp, compare, equal, quickcheck]

    include Op_types.S_binary with type t := t
  end

  (** {3 Logical binary operators} *)

  module Logical : sig
    (** Enumeration of logical binary operators. *)
    type t =
      | And  (** A logical AND operator. *)
      | Or  (** A logical OR operator. *)
    [@@deriving sexp, compare, equal, quickcheck]

    include Op_types.S_binary with type t := t
  end

  (** {3 Relational binary operators} *)

  module Rel : sig
    (** Enumeration of relational binary operators. *)
    type t =
      | Eq  (** An equality operator. *)
      | Ne  (** A non-equality operator. *)
    [@@deriving sexp, compare, equal, quickcheck]

    include Op_types.S_binary with type t := t
  end

  (** {3 Main enumeration} *)

  (** Enumeration of binary operators. *)
  type t =
    | Rel of Rel.t  (** Lifts a relational operator to a binary operator. *)
    | Arith of Arith.t
        (** Lifts an arithmetic operator to a binary operator. *)
    | Bitwise of Bitwise.t
        (** Lifts a bitwise operator to a binary operator. *)
    | Logical of Logical.t
        (** Lifts a logical operator to a binary operator. *)
  [@@deriving sexp, compare, equal, quickcheck]

  include Op_types.S_binary with type t := t

  (** {4 Convenience constructors} *)

  val eq : t
  (** [eq] is the equality operator. *)

  val ne : t
  (** [ne] is the non-equality operator. *)

  val add : t
  (** [add] is the addition operator. *)

  val sub : t
  (** [sub] is the subtraction operator. *)

  val b_and : t
  (** [b_and] is the bitwise AND operator. *)

  val b_or : t
  (** [b_or] is the bitwise OR operator. *)

  val b_xor : t
  (** [b_xor] is the bitwise XOR operator. *)

  val l_and : t
  (** [l_and] is the logical AND operator. *)

  val l_or : t
  (** [l_or] is the logical OR operator. *)

  include Op_types.S_binary with type t := t
end

(** {1 Other operators} *)

(** {2 Atomic fetch operators}

    The c-mini representation of fetch-and-X instructions treats each a
    variant of the same 'fetch' instruction, with the X disambiguated by an
    extra [Fetch.t] parameter. *)
module Fetch : sig
  (** The enumeration of fetch postfix operations. *)
  type t =
    | Add  (** Fetch and add. *)
    | Sub  (** Fetch and subtract. *)
    | And  (** Fetch and (bitwise) AND. *)
    | Or  (** Fetch and (bitwise) OR. *)
    | Xor  (** Fetch and (bitwise) XOR. *)

  val to_bop : t -> Binary.t
  (** [to_bop op] gets the binary operator that represents the operation that
      [op] represents between the old value and the fetch argument. *)

  include Act_utils.Enum_types.S_table with type t := t

  include Act_utils.Enum_types.Extension_table with type t := t

  include Op_types.S_binary with type t := t

  (** {3 Prebuilt quickcheck modules} *)

  (** A restricted form of the fetch generator that generates only operators
      for which the fetch argument [0] doesn't change the fetch object. *)
  module Gen_idem_zero_rhs :
    Act_utils.My_quickcheck.S_with_sexp with type t = t

  (** A restricted form of the fetch generator that generates only operators
      for which a fetch argument that evaluates to the same value as the
      fetch object doesn't change the fetch object. *)
  module Gen_idem_refl : Act_utils.My_quickcheck.S_with_sexp with type t = t
end
