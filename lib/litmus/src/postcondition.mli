(* This file is part of c4f.

   Copyright (c) 2018-2021 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Minimalistic AST for litmus test postconditions.

    This AST only tries to cover the parts of the litmus-test format that ACT
    actually needs for input.

    These parts of the litmus AST have no module-level dependency on the
    underlying language of the litmus tests. *)

open Base

(** {1 Quantifiers} *)

module Quantifier : sig
  (** The variant type of quantifiers. *)
  type t = Exists | For_all [@@deriving quickcheck]

  include Act_utils.Enum_types.Extension_table with type t := t
end

(** {1 Postconditions proper} *)

(** Type of Litmus postconditions. *)
type 'const t [@@deriving sexp, compare, equal, quickcheck]

(** {2 Constructors} *)

val make :
  quantifier:Quantifier.t -> predicate:'const Predicate.t -> 'const t
(** [make ~quantifier ~predicate] constructs a postcondition. *)

val exists : 'const Predicate.t -> 'const t
(** [exists predicate] is shorthand for constructing an existentially
    quantified predicate. *)

val for_all : 'const Predicate.t -> 'const t
(** [for_all predicate] is shorthand for constructing a universally
    quantified predicate. *)

(** {2 Accessors} *)

val quantifier : 'const t -> Quantifier.t
(** [quantifier post] gets [post]'s quantifier. *)

val predicate : 'const t -> 'const Predicate.t
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
