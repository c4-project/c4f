(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

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
  (** [of_string_exn str] behaves as [of_string str], but raises an
      exception if the string isn't in the table. *)

  val to_string : t -> string option
  (** [to_string t] looks up the string equivalent of [t] in the string
      table. *)

  val to_string_exn : t -> string
  (** [to_string_exn t] behaves as [to_string t], but raises an exception if
      [t] has no string in the table. . *)
end

module Make (T : sig
  type t [@@deriving equal]

  include Table with type t := t
end) : S with type t = T.t
(** [Make] lifts a [Table] into a module satisfying [Intf]. *)

module To_stringable (T : S) : Stringable.S with type t := T.t
(** [To_stringable] produces a plain stringable instance given a string
    table. *)
