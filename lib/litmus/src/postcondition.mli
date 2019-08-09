(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Litmus AST: base modules and functors.

    These parts of the litmus AST have no module-level dependency on the
    underlying language of the litmus tests. *)

open Base

(** {1 Predicate elements} *)

(** Directly-parametrised AST for basic predicate elements.

    The distinction between [Pred_elt] and {{!Pred} Pred} mainly exists to
    make conversion to and from other languages, like [Blang], easier. *)
module Pred_elt : sig
  type 'const t = Eq of Act_common.Litmus_id.t * 'const
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

(** {1 Predicates} *)

(** Directly-parametrised AST for predicates. *)
module Pred : sig
  (** Type of Litmus predicates. *)
  type 'const t =
    | Bracket of 'const t
    | Or of 'const t * 'const t
    | And of 'const t * 'const t
    | Elt of 'const Pred_elt.t
  [@@deriving sexp, compare, equal, quickcheck]

  (** {2 Constructors} *)

  module Infix : sig
    val ( ==? ) : Act_common.Litmus_id.t -> 'const -> 'const t
    (** [k ==? v] is a primitive equality test that key [k] maps to value
        [v]. *)

    val ( && ) : 'const t -> 'const t -> 'const t
    (** [l && r] is logical conjunction, representing the /\ operator in
        written Litmus. *)

    val ( || ) : 'const t -> 'const t -> 'const t
    (** [l || r] is logical disjunction, representing the \/ operator in
        written Litmus. *)
  end

  val elt : 'const Pred_elt.t -> 'const t
  (** [elt x] lifts [x] to a predicate. *)

  val bracket : 'const t -> 'const t
  (** [bracket x] surrounds [x] with parentheses. *)

  val debracket : 'const t -> 'const t
  (** [debracket pred] removes any brackets in [pred]. *)

  (** {2 Traversals} *)

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
end

(** {1 Quantifiers} *)

module Quantifier : sig
  (** The variant type of quantifiers. *)
  type t = Exists | For_all [@@deriving quickcheck]

  include Act_utils.Enum_types.Extension_table with type t := t
end

(** {1 Postconditions proper} *)

type 'const t [@@deriving sexp, compare, equal, quickcheck]
(** Type of Litmus postconditions. *)

val make : quantifier:Quantifier.t -> predicate:'const Pred.t -> 'const t
(** [make ~quantifier ~predicate] constructs a postcondition. *)

val quantifier : 'const t -> Quantifier.t
(** [quantifier post] gets [post]'s quantifier. *)

val predicate : 'const t -> 'const Pred.t
(** [predicate post] gets [post]'s predicate. *)

(** {2 Traversals} *)

(** Bi-traversing monadically over all Litmus identifiers in a predicate on
    the left, and all constants on the right. *)
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
