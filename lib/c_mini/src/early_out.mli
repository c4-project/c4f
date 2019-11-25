(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-C: 'early out' statements.

    An 'early out' statement is a {{!Prim_statement} primitive statement}
    that represents an abrupt break in control flow. These are:

    - breaks;
    - returns (Mini-C doesn't support valued return statements). *)

open Base

(** {1 Kinds of early-out} *)

module Kind : sig
  (** Type of early-out kinds. *)
  type t = Break | Return [@@deriving sexp, equal, quickcheck]
end

(** {1 Early-out statements} *)

type 'meta t [@@deriving sexp, equal]
(** Opaque type of mini-C early-out statements, parametrised by metadata. *)

(** {2 Constructors} *)

val make : meta:'meta -> kind:Kind.t -> 'meta t
(** [make ~meta ~kind] creates an early-out statement with kind [kind] and
    metadata [meta]. *)

val break : 'meta -> 'meta t
(** [break m] creates a break statement with metadata [m]. *)

val return : 'meta -> 'meta t
(** [return m] creates a return statement with metadata [m]. *)

(** {2 Accessors} *)

val meta : 'meta t -> 'meta
(** [kind x] gets [x]'s metadata. *)

(** {3 Kinds} *)

val kind : 'meta t -> Kind.t
(** [kind x] gets the kind of early-out that [x] is. *)

(** {2 Traversals} *)

module On_meta : Travesty.Traversable_types.S1 with type 'meta t := 'meta t
(** A traversal over the metadata in a primitive statement. *)
