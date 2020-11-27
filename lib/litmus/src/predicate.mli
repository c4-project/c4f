(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Minimalistic AST for litmus test predicates. *)

open Base

(** {1 Predicate elements} *)

(** Directly-parametrised AST for basic predicate elements.

    The distinction between [Pred_elt] and {{!Pred} Pred} mainly exists to
    make conversion to and from other languages, like [Blang], easier. *)
module Element : sig
  type 'const t = Bool of bool | Eq of Act_common.Litmus_id.t * 'const
  [@@deriving sexp, compare, equal, quickcheck]

  (** {2 Constructors} *)

  val ( ==? ) : Act_common.Litmus_id.t -> 'const -> 'const t

  (** {2 Traversals} *)

  (** Bi-traversing monadically over all identifiers in a predicate element
      on the left, and all constants on the right. *)
  include
    Travesty.Bi_traversable_types.S1_right
      with type 'c t := 'c t
       and type left = Act_common.Litmus_id.t

  (** Bi-traversing monadically over all C identifiers in a predicate on the
      left, and all constants on the right. *)
  module On_c_identifiers :
    Travesty.Bi_traversable_types.S1_right
      with type 'c t = 'c t
       and type left = Act_common.C_id.t
end

(** {1 Predicate binary operators} *)

module Bop : sig
  (** Type of predicate binary operators. *)
  type t = Or | And
end

(** {1 Predicates} *)

(** Type of Litmus predicates. *)
type 'const t [@@deriving sexp, compare, equal, quickcheck]

(** {2 Constructors} *)

(** {3 Disjunction} *)

val or_ : 'const t -> 'const t -> 'const t
(** [or_ l r] is logical disjunction, representing the '\/' operator in
    written Litmus. *)

val optimising_or : 'const t -> 'const t -> 'const t
(** [optimising_or l r] is [or_ l r], but short-circuits in various cases. *)

val optimising_or_seq : 'const t Sequence.t -> 'const t
(** [optimising_or_seq xs] disjoins all of the elements in [xs] using
    [optimising_or]. *)

(** {3 Conjunction} *)

val and_ : 'const t -> 'const t -> 'const t
(** [and_ l r] is logical conjunction, representing the '/\' operator in
    written Litmus. *)

val optimising_and : 'const t -> 'const t -> 'const t
(** [optimising_and l r] is [and_ l r], but short-circuits in various cases. *)

val optimising_and_seq : 'const t Sequence.t -> 'const t
(** [optimising_and_seq xs] conjoins all of the elements in [xs] using
    [optimising_and]. *)

val eq : Act_common.Litmus_id.t -> 'const -> 'const t
(** [eq k v] is a primitive equality test that key [k] maps to value [v]. *)

val bool : bool -> 'const t
(** [bool b] lifts [b] to a predicate. *)

(** {3 Infix constructors} *)

module Infix : sig
  val ( ==? ) : Act_common.Litmus_id.t -> 'const -> 'const t
  (** [k ==? v] is [eq k v]. *)

  val ( && ) : 'const t -> 'const t -> 'const t
  (** [l && r] is [and l r]. *)

  val ( &&+ ) : 'const t -> 'const t -> 'const t
  (** [l ||+ r] is [optimising_and l r]. *)

  val ( || ) : 'const t -> 'const t -> 'const t
  (** [l || r] [or l r]. *)

  val ( ||+ ) : 'const t -> 'const t -> 'const t
  (** [l ||+ r] is [optimising_or l r]. *)
end

val elt : 'const Element.t -> 'const t
(** [elt x] lifts [x] to a predicate. *)

(** {2 Traversals} *)

val reduce :
     'const t
  -> elt:('const Element.t -> 'a)
  -> bop:(Bop.t -> 'a -> 'a -> 'a)
  -> 'a
(** [reduce x ~elt ~bop] reduces [x] to a single value by using [elt] to
    convert elements to values and [bop] to combine reductions over binary
    operators. *)

(** Bi-traversing monadically over all identifiers in a predicate on the
    left, and all constants on the right. *)
include
  Travesty.Bi_traversable_types.S1_right
    with type 'c t := 'c t
     and type left = Act_common.Litmus_id.t

(** Bi-traversing monadically over all C identifiers in a predicate on the
    left, and all constants on the right. *)
module On_c_identifiers :
  Travesty.Bi_traversable_types.S1_right
    with type 'c t = 'c t
     and type left = Act_common.C_id.t

(** {2 Pretty-printing} *)

val pp : Formatter.t -> 'const t -> pp_const:'const Fmt.t -> unit
(** [pp f pred ~pp_const] pretty-prints [pred] on [f], using [pp_const] to
    print constants. *)
