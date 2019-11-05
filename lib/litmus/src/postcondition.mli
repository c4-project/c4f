(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
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

type 'const t [@@deriving sexp, compare, equal, quickcheck]
(** Type of Litmus postconditions. *)

val make :
  quantifier:Quantifier.t -> predicate:'const Predicate.t -> 'const t
(** [make ~quantifier ~predicate] constructs a postcondition. *)

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
