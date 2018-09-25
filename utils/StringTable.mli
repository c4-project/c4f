(* This file is part of 'act'.

Copyright (c) 2018 by Matt Windsor

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

(** Case-insentitive bidirectional string lookup table modules *)

open Core

(** [Table] is a signature containing the raw string table itself. *)
module type Table =
  sig
    type t

    val table : (t, string) List.Assoc.t
  end

(** [Intf] is the abstract interface of the string table. *)
module type Intf =
  sig
    type t

    (** [of_string str] tries to look up the string [str] in the
       string table.

       All lookups are case-insensitive.

       [of_string] may raise an exception if the original table was
       badly formed. *)
    val of_string : string -> t option

    (** [of_string_exn str] behaves as [of_string str], but raises an
       exception if the string isn't in the table. *)
    val of_string_exn : string -> t

    (** [to_string ?equal t] looks up the string equivalent of [t] in
       the string table.

       If provided, it uses [equal] as the comparator; it defaults
       to [=]. *)
    val to_string : ?equal:(t -> t -> bool) -> t -> string option

    (** [to_string_exn ?equal t] behaves as [to_string ?equal t], but
     raises an exception if [t] has no string in the table.  . *)
    val to_string_exn : ?equal:(t -> t -> bool) -> t -> string
  end

(** [Make] lifts a [Table] into a module satisfying [Intf]. *)
module Make : functor (T : Table) -> (Intf with type t = T.t)
