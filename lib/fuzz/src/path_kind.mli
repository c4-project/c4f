(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Kinds of path. *)

(** Enumeration of kinds of path.

    Most of the path producer code is uniform across all path kinds, and so
    we use a single generator with a kind enumeration rather than separate
    producers. *)
type t =
  | Insert  (** Paths that insert a statement into a block. *)
  | Transform  (** Paths that transform a statement. *)
  | Transform_list
      (** Paths that transform a list of statements in a block. *)

include Act_utils.Enum_types.Extension_table with type t := t

(** {1 With actions}

    Kinds with actions are used in path consumers. *)

module With_action : sig
  (** Type of transformation functions. *)
  type 'a transformer = 'a -> 'a Base.Or_error.t

  type kind := t

  type t =
    | Insert of Subject.Statement.t list
    | Transform of Subject.Statement.t transformer
    | Transform_list of Subject.Statement.t list transformer
        (** Enumeration of kinds of path, complete with actions. *)

  val to_kind : t -> kind
  (** [to_kind ka] erases [ka]'s action, turning it into a kind which can be
      stringified, sexpified, and so on. *)
end
