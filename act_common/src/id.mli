(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to permit
   persons to whom the Software is furnished to do so, subject to the
   following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
   NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
   DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
   OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
   USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** [Id] is a module for compiler and machine identifiers.

    Identifiers contain an ordered list of case-insensitive elements, called
    'tags'. *)

open Core_kernel

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

(** We can use [t] as an [Identifiable]. *)
include Identifiable.S with type t := t

(** {2 Operators for building identifiers} *)

val ( @: ) : string -> t -> t
(** [x @: y] pushes a tag [x] onto identifier [y]. *)

val ( @. ) : t -> t -> t
(** [x @. y] concatenates identifiers [x] and [y]. *)

(** {2 Pretty-printing helpers} *)

val pp_alist : 'e Fmt.t -> (t, 'e) List.Assoc.t Fmt.t
(** [pp_alist ppe] vertically pretty-prints an associative list from
    identifiers to values printable by [ppe]. *)

val pp_map : 'e Fmt.t -> 'e Map.t Fmt.t
(** [pp_map ppe] vertically pretty-prints a map from identifiers to values
    printable by [ppe]. *)

(** {2 Property language} *)

(** [Property] contains a mini-language for querying IDs, suitable for use
    in [Blang]. *)
module Property : sig
  (** [id] is a synonym for the identifier type. *)
  type id = t

  (** [t] is the opaque type of property queries. *)
  type t [@@deriving sexp]

  val has_tag : string -> t
  (** [has_tag s str] constructs a membership test over a string [str]. *)

  val is : string -> t
  (** [is str] constructs an equality test over a string [str]. *)

  val has_prefix : string -> t
  (** [has_prefix str] constructs a prefix test over a string [str]. *)

  val eval : id -> t -> bool
  (** [eval id property] decides whether [property] holds for [id]. *)

  val eval_b : id -> t Blang.t -> bool
  (** [eval_b id expr] evaluates a [Blang] expression [expr] over [id]. *)

  val pp_tree : unit Fmt.t
  (** [pp_tree f ()] prints a tree describing the property language on [f]. *)
end
