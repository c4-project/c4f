(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Module types shared by {!Statement_class}, {!Expression_class}, and their
    subclasses.

    These types describe how to 'classify' a particular statement,
    expression, and so on. This, in turn, allows for matching these elements
    based on class, or counting all of the elements that match a class, and
    so on. *)

open Base

(** Module type of base classifications. *)
module type S = sig
  (** Enumeration of primitive statement classes. *)
  type t

  (** The element being classified, which may range over metadata. *)
  type 'meta elt

  val classify : 'meta elt -> t option
  (** [classify s] tries to classify [s] without recursion. *)

  val classify_rec : 'meta elt -> t list
  (** [classify_rec s] recursively classifies every classifiable element in
      [s], returning a multiset of classifications expressed as a list. *)

  val class_matches : t -> template:t -> bool
  (** [class_matches clazz ~template] checks whether [clazz] matches
      [template]. Holes in [template] match any corresponding class in
      [clazz], but not vice versa. *)
end

(** Module type of extended classifications. *)
module type S_ext = sig
  include S

  val count_rec_matches : 'meta elt -> templates:t list -> int
  (** [count_rec_matches elt ~templates] counts the recursive number of times
      that any class in [templates] matches [elt] or any of its
      subcomponents. *)

  val class_matches_any : t -> templates:t list -> bool
  (** [class_matches_any clazz ~templates] checks whether [clazz] {!matches}
      any of the templates in [templates]. *)

  val class_unmatches_any : t -> templates:t list -> bool
  (** [class_matches_any clazz ~templates] checks whether [clazz] fails to
      match at least one of the templates in [templates]. *)

  val matches_any : 'e elt -> templates:t list -> bool
  (** [matches_any elt ~templates] checks whether [elt]'s class directly
      {!matches} any of the templates in [templates]. *)

  val unmatches_any : 'e elt -> templates:t list -> bool
  (** [unmatches_any elt ~templates] checks whether [elt]'s class fails to
      match directly at least one of the templates in [templates]. *)

  val rec_matches_any : 'e elt -> templates:t list -> bool
  (** [rec_matches_any elt ~templates] checks whether [elt]'s class
      recursively {!matches} any of the templates in [templates]. *)

  val rec_unmatches_any : 'e elt -> templates:t list -> bool
  (** [rec_unmatches_any elt ~templates] checks whether [elt]'s class
      recursively contains a classification that fails to match at least one
      of the templates in [templates]. *)
end
