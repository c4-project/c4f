(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini C: elements tagged with a C identifier. *)

open Base

type 'a t [@@deriving sexp, equal]
(** Opaque type of named elements. *)

(** {2 Constructors} *)

val make : 'a -> name:Act_common.C_id.t -> 'a t
(** [make value ~name] tags [value] with identifier [name]. *)

val seq_of_alist : (Act_common.C_id.t, 'a) List.Assoc.t -> 'a t Sequence.t
(** [seq_of_alist xs] converts an identifier-keyed association list [xs]
    to a sequence of named values. *)

val list_of_alist : (Act_common.C_id.t, 'a) List.Assoc.t -> 'a t list
(** [list_of_alist xs] converts an identifier-keyed association list [xs]
    to a list of named values. *)

(** {2 Projections} *)

val name : _ t -> Act_common.C_id.t
(** [name n] gets the name tag of [n]. *)

val value : 'a t -> 'a
(** [value n] gets the underlying value of [n]. *)

val alist_of_seq : 'a t Sequence.t -> (Act_common.C_id.t, 'a) List.Assoc.t
(** [alist_of_seq xs] converts a sequence of named values [xs] to an
   identifier-keyed association list. *)

val alist_of_list : 'a t list -> (Act_common.C_id.t, 'a) List.Assoc.t
(** [list_of_alist xs] converts an identifier-keyed association list [xs]
    to a list of named values. *)

(** {2 Traversing} *)

include Travesty.Bi_traversable_types.S1_right with type 'r t := 'r t and
type left = Act_common.C_id.t

(** {2 Helpers for associative lists of C identifiers and values} *)

module Alist : sig
  module As_named (A : Equal.S) : Travesty.Traversable_types.S0
    with type Elt.t = A.t t
     and type t = (Act_common.C_id.t, A.t) List.Assoc.t

  type 'a t = (Act_common.C_id.t, 'a) List.Assoc.t [@@deriving sexp, equal]
end
