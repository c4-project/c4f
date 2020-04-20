(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** C-mini: operators *)

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
  end

  (** {3 Logical binary operators} *)

  module Logical : sig
    (** Enumeration of logical binary operators. *)
    type t =
      | And  (** A logical AND operator. *)
      | Or  (** A logical OR operator. *)
    [@@deriving sexp, compare, equal, quickcheck]
  end

  (** Enumeration of binary operators. *)
  type t =
    | Eq  (** An equality operator. *)
    | Arith of Arith.t
        (** Lifts an arithmetic operator to a binary operator. *)
    | Logical of Logical.t
        (** Lifts a logical operator to a binary operator. *)
  [@@deriving sexp, compare, equal, quickcheck]

  val eq : t
  (** [eq] is the equality operator. *)

  val add : t
  (** [add] is the addition operator. *)

  val sub : t
  (** [sub] is the subtraction operator. *)

  val l_and : t
  (** [l_and] is the logical AND operator. *)

  val l_or : t
  (** [l_or] is the logical OR operator. *)
end

(** {1 Other operators} *)

(** {2 Atomic fetch operators}

    The c-mini representation of fetch-and-X instructions treats each a
    variant of the same 'fetch' instruction, with the X disambiguated by an
    extra [Fetch.t] parameter. *)
module Fetch : sig
  (** The enumeration of fetch postfix operations. *)
  type t = Add  (** Fetch and add. *) | Sub  (** Fetch and subtract. *)

  val to_bop : t -> Binary.t
  (** [to_bop op] gets the binary operator that represents the operation that
      [op] represents between the old value and the fetch argument. *)

  include Act_utils.Enum_types.S_table with type t := t

  include Act_utils.Enum_types.Extension_table with type t := t
end
