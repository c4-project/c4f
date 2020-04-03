(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A non-empty list. *)

open Base

(** Type of non-empty lists. *)
type 'a t

(** {1 Constructors} *)

val make : head:'a -> tail:'a list -> 'a t
(** [make ~head ~tail] makes a non-empty list from a head [head] and tail
    [tail]. *)

val cons : 'a -> 'a t -> 'a t
(** [cons x xs] pushes [xs] onto the head of the non-empty list [xs]. *)

(** {2 Constructing from lists} *)

val of_list_err : 'a list -> 'a t Or_error.t
(** [of_list_err xs] returns the non-empty form of [xs] if [xs] is non-empty,
    or a descriptive error otherwise. *)

val of_list_opt : 'a list -> 'a t option
(** [of_list_opt xs] behaves as [of_list_err xs], but returns the result as
    an option rather than an [Or_error.t]. *)

val of_list_exn : 'a list -> 'a t
(** [of_list_exn xs] behaves as [of_list_err xs], but raises an exception if
    the list is non-empty. *)

(** {1 Accessors} *)

val head : 'a t -> 'a
(** [head xs] gets the head of [xs]. *)

val tail : 'a t -> 'a list
(** [tail xs] gets the (possibly empty) tail of [xs]. *)

val to_list : 'a t -> 'a list
(** [to_list xs] returns [xs] as a list. This always succeeds. *)
