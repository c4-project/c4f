(* The Automagic Compiler Tormentor

   Copyright (c) 2018, 2019, 2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Case-insentitive bidirectional string lookup table modules *)

open Base

(** [Table] is a signature containing the raw string table itself. *)
module type Table = sig
  type t

  val table : (t, string) List.Assoc.t
  (** [table] is the string table, mapping each [t] to a string. *)
end

(** [S] is the signature of string tables built with [Make]. *)
module type S = sig
  (* This lets us access the table directly. *)
  include Table

  val of_string : string -> t option
  (** [of_string str] tries to look up the string [str] in the string table.

      All lookups are case-insensitive.

      [of_string] may raise an exception if the original table was badly
      formed. *)

  val of_string_exn : string -> t
  (** [of_string_exn str] behaves as [of_string str], but raises an exception
      if the string isn't in the table. *)

  val to_string : t -> string option
  (** [to_string t] looks up the string equivalent of [t] in the string
      table. *)

  val to_string_exn : t -> string
  (** [to_string_exn t] behaves as [to_string t], but raises an exception if
      [t] has no string in the table. . *)
end

(** [Make] lifts a [Table] into a module satisfying [Intf]. *)
module Make (T : sig
  type t [@@deriving equal]

  include Table with type t := t
end) : S with type t = T.t

(** [To_stringable] produces a plain stringable instance given a string
    table. *)
module To_stringable (T : S) : Stringable.S with type t := T.t
