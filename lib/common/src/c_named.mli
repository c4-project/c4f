(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** FIR: elements tagged with a C identifier. *)

open Base

(** Type of named elements. *)
type 'a t = {name: C_id.t; value: 'a}
[@@deriving accessors, sexp, compare, equal, quickcheck]

(** {2 Constructors} *)

val make : 'a -> name:C_id.t -> 'a t
(** [make value ~name] tags [value] with identifier [name]. *)

val seq_of_alist : (C_id.t, 'a) List.Assoc.t -> 'a t Sequence.t
(** [seq_of_alist xs] converts an identifier-keyed association list [xs] to a
    sequence of named values. *)

val list_of_alist : (C_id.t, 'a) List.Assoc.t -> 'a t list
(** [list_of_alist xs] converts an identifier-keyed association list [xs] to
    a list of named values. *)

val alist_of_seq : 'a t Sequence.t -> (C_id.t, 'a) List.Assoc.t
(** [alist_of_seq xs] converts a sequence of named values [xs] to an
    identifier-keyed association list. *)

val alist_of_list : 'a t list -> (C_id.t, 'a) List.Assoc.t
(** [list_of_alist xs] converts an identifier-keyed association list [xs] to
    a list of named values. *)

(** {2 Traversing} *)

include
  Travesty.Bi_traversable_types.S1_right
    with type 'r t := 'r t
     and type left = C_id.t

(** {2 Helpers for associative lists of C identifiers and values} *)

module Alist : sig
  module As_named (A : Equal.S) :
    Travesty.Traversable_types.S0
      with type Elt.t = A.t t
       and type t = (C_id.t, A.t) List.Assoc.t

  type 'a t = (C_id.t, 'a) List.Assoc.t [@@deriving sexp, equal]
end
