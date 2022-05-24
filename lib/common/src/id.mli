(* This file is part of c4f.

   Copyright (c) 2018-2022 C4 Project

   c4t itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   Parts of c4t are based on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** [Id] is a module for compiler and machine identifiers.

    Identifiers contain an ordered list of case-insensitive elements, called
    'tags'. *)

open Base

(** [t] is the type of compiler and machine identifiers. *)
type t

(** {2 Constructing identifiers}

    See also [Identifiable.S]. *)

(** {2 Destructing identifiers}

    See also [Identifiable.S]. *)

val hd_reduce : t -> on_empty:(unit -> 'a) -> f:(string -> t -> 'a) -> 'a
(** [hd_reduce id ~on_empty ~f] applies [on_empty ()] if [id] is empty, or
    [f tag id'] if [id] comprises a head tag [tag] and trailing id [id']. *)

val of_string_list : string list -> t
(** [of_string_list tags] produces an identifier from a tag list. *)

val to_string_list : t -> string list
(** [to_string_list id] returns a list of each tag in [id]. *)

val has_tag : t -> string -> bool
(** [has_tag id tag] decides whether [id] contains the tag [tag], modulo
    case. *)

val is_prefix : t -> prefix:t -> bool
(** [is_prefix id ~prefix] decides whether [prefix] is a prefix of [id].

    An ID is a prefix of another ID if its list of tags is a prefix of the
    other ID's list of tags, modulo case. *)

val drop_prefix : t -> prefix:t -> t Or_error.t
(** [drop_prefix id ~prefix] returns [id] with [prefix] removed, or an error
    if [is_prefix id ~prefix] is false. *)

val try_find_assoc_with_suggestions :
  (t, 'a) List.Assoc.t -> t -> id_type:string -> 'a Or_error.t
(** [try_find_assoc_with_suggestions assoc id ~id_type] tries to find an ID
    [id] in an associative list [assoc]. If it can't find one, it returns an
    error mentioning [id_type], with edit-distance-based suggestions drawn
    from [assoc]. *)

(* TODO(@MattWindsor91): this should get the first LONGEST prefix, not the
   first prefix. *)

val try_find_assoc_with_suggestions_prefix :
  (t, 'a) List.Assoc.t -> t -> id_type:string -> (t * 'a) Or_error.t
(** [try_find_assoc_with_suggestions_prefix assoc id ~id_type] behaves as
    [try_find_assoc_with_suggestions], but finds the first element of [assoc]
    whose ID is a prefix of [id], and returns that ID alongside the result. *)

(** We can use [t] as an [Identifiable]. *)
include Identifiable.S with type t := t

(** {2 Operators for building identifiers} *)

val empty : t
(** [empty] is the empty identifier. *)

val ( @: ) : string -> t -> t
(** [x @: y] pushes a tag [x] onto identifier [y]. *)

val ( @. ) : t -> t -> t
(** [x @. y] concatenates identifiers [x] and [y]. *)

(** {2 Pretty-printing helpers} *)

val pp_alist : 'e Fmt.t -> (t, 'e) List.Assoc.t Fmt.t
(** [pp_alist ppe] vertically pretty-prints an associative list from
    identifiers to values printable by [ppe]. *)

val pp_map : 'e Fmt.t -> (t, 'e, comparator_witness) Map.t Fmt.t
(** [pp_map ppe] vertically pretty-prints a map from identifiers to values
    printable by [ppe]. *)
