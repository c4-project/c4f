(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Mini-C: labels.

    The label structure is used both for labels and gotos. *)

type 'meta t [@@deriving sexp, compare, equal]
(** Type of labels, parametrised over their metadata. *)

(** {1 Constructors} *)

val make : meta:'meta -> name:Act_common.C_id.t -> 'meta t
(** [make ~meta ~name] makes a label with the metadata [meta] and name
    [name]. *)

val of_c_id : Act_common.C_id.t -> unit t
(** [of_c_id name] makes a metadata-less label with the name [name]. *)

(** {1 Accessors} *)

val meta : 'meta t -> 'meta
(** [meta label] gets [label]'s metadata. *)

val name : _ t -> Act_common.C_id.t
(** [name label] gets [label]'s name. *)

(** {1 Traversals} *)

module On_meta : Travesty.Traversable_types.S1 with type 'meta t = 'meta t
(** [On_meta] permits traversal over a label's metadata. *)
