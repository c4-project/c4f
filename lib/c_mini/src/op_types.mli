(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Signatures for operation modules *)

(** Basic type of binary operation modules. *)
module type S_binary = sig
  (** The type of operation. *)
  type t [@@deriving sexp, compare, equal, quickcheck]

  val refl_zero : t -> bool
  (** [refl_zero op] retrieves whether [x op x] equals [0]. *)

  val zero_lhs_unit : t -> bool
  (** [zero_lhs_unit op] retrieves whether [0 op x] equals [x]. *)

  val zero_rhs_unit : t -> bool
  (** [zero_rhs_unit op] retrieves whether [x op 0] equals [x]. *)
end
