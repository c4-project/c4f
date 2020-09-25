(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Basic wrapper for metadata-carrying FIR nodes.

    This wrapper is useful for attaching metadata to sum types; generally,
    records that can contain metadata will do so inline. *)

open Base
open Import

(** Type of metadata wrappers. *)
type ('m, 'v) t =
  { meta: 'm
  ; value: 'v
  }
  [@@deriving sexp, compare, equal, accessors]

val make : 'v -> meta:'m -> ('m, 'v) t
(** [make value ~meta] is a convenience constructor for metadata-carrying values. *)

val no_meta: ('i -> 'v1 -> 'v2, 'i -> (unit, 'v1) t -> (unit, 'v2) t, [<isomorphism]) Accessor.t
(** [no_meta] draws an isomorphism between raw values and empty metadata-carried forms. *)

include Travesty.Bi_traversable_types.S2 with type ('m, 'v) t := ('m, 'v) t
(** We can bi-traverse metadata wrappers. *)