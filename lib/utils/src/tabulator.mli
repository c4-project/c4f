(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person obtaining a
   copy of this software and associated documentation files (the "Software"),
   to deal in the Software without restriction, including without limitation
   the rights to use, copy, modify, merge, publish, distribute, sublicense,
   and/or sell copies of the Software, and to permit persons to whom the
   Software is furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE. *)

(** [Tabulator] is an abstract data type for building tables of
    pretty-printed values. *)

open Base

(** [t] is the opaque type of a tabulator. *)
type t

(** [row] is the type of a tabulator row. *)
type row = string list

val make :
  ?sep:string -> ?terminator:string -> header:row -> unit -> t Or_error.t
(** [make ?sep ?terminator ~header] builds a tabulator with the header row
    [header], optionally using [sep] to separate columns and [terminator] to
    terminate each row. It fails if [header] is empty. *)

val add_row : t -> row:row -> t Or_error.t
(** [add_row t ~row] adds [row] to the tabulator [t], returning the resulting
    tabulator. It fails if [row] is a different length from [t]'s header row. *)

val add_rows : t -> rows:row list -> t Or_error.t
(** [add_rows t ~rows] adds [rows] to the tabulator [t] in order, returning
    the resulting tabulator. It fails if any row in [rows] is a different
    length from [t]'s header row. *)

val add_rule : ?char:char -> t -> t Or_error.t
(** [add_rule ?char t] adds a dividing rule made up of [char] (default '-')
    to tabulator [t], returning the resulting tabulator. *)

val print : ?oc:Stdio.Out_channel.t -> t -> unit
(** [print t] prints [t] on [oc] (default stdout). *)

(** [Tabular] is a signature for abstract data types that can be tabulated
    using a [Tabulator]. *)
module type Tabular = sig
  type data

  val to_table : data -> t Or_error.t
  (** [to_table t] converts [t] into a tabular representation, or returns an
      error trying. *)
end

(** [Tabular_extensions] contains extensions for [Tabular]. To create one,
    use [Extend_tabular]. *)
module type Tabular_extensions = sig
  type data

  val print_as_table :
    ?oc:Stdio.Out_channel.t -> ?on_error:(Error.t -> unit) -> data -> unit
  (** [pp_as_table ?on_error t] tries to convert [t] to a table and
      pretty-print it on [oc] (default stdout). If the data can't be shown in
      table format, [?on_error] (or, if missing, a default handler) is
      instead used. *)
end

(** [Extend_tabular] makes a [Tabular_extensions] out of a [Tabular]. *)
module Extend_tabular (T : Tabular) :
  Tabular_extensions with type data := T.data
