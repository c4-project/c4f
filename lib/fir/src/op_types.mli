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
  type t [@@deriving enumerate, sexp, compare, equal, quickcheck]

  val rules : t -> Op_rule.t list
  (** [rules op] gets the algebraic rules defined for [op]. *)
end
