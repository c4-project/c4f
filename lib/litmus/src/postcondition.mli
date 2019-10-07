(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Minimalistic AST for litmus test postconditions.

    This AST only tries to cover the parts of the litmus-test format that
    ACT actually needs for input.

    These parts of the litmus AST have no module-level dependency on the
    underlying language of the litmus tests. *)

open Base

(** {1 Predicate elements} *)

(** Directly-parametrised AST for basic predicate elements.

    The distinction between [Pred_elt] and {{!Pred} Pred} mainly exists to
    make conversion to and from other languages, like [Blang], easier. *)
module Pred_elt : sig
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

module Pred_bop : sig
  (** Type of predicate binary operators. *)
  type t = Or | And
end

(** {1 Predicates} *)

(** Directly-parametrised AST for predicates. *)
module Pred : sig
  type 'const t [@@deriving sexp, compare, equal, quickcheck]
  (** Type of Litmus predicates. *)

  (** {2 Constructors} *)

  val or_ : 'const t -> 'const t -> 'const t
  (** [or_ l r] is logical disjunction, representing the '\/' operator in
      written Litmus. *)

  val optimising_or : 'const t -> 'const t -> 'const t
  (** [optimising_or l r] is [or_ l r], but short-circuits in various cases. *)

  val and_ : 'const t -> 'const t -> 'const t
  (** [and_ l r] is logical conjunction, representing the '/\' operator in
      written Litmus. *)

  val optimising_and : 'const t -> 'const t -> 'const t
  (** [optimising_and l r] is [and_ l r], but short-circuits in various
      cases. *)

  val eq : Act_common.Litmus_id.t -> 'const -> 'const t
  (** [eq k v] is a primitive equality test that key [k] maps to value [v]. *)

  val bool : bool -> 'const t
  (** [bool b] lifts [b] to a predicate. *)

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

  val elt : 'const Pred_elt.t -> 'const t
  (** [elt x] lifts [x] to a predicate. *)

  (** {2 Traversals} *)

  val reduce :
       'const t
    -> elt:('const Pred_elt.t -> 'a)
    -> bop:(Pred_bop.t -> 'a -> 'a -> 'a)
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
