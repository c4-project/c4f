(* This file is part of 'act'.

   Copyright (c) 2018 by Matt Windsor

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

(** [Tabulator] is an abstract data type for building tables of
   pretty-printed values. *)

open Core

(** [t] is the opaque type of a tabulator. *)
type t

(** [row] is the type of a tabulator row. *)
type row = (Format.formatter -> unit) list

(** [make ?sep ?terminator ~header] builds a tabulator with the header
   row [header], optionally using [sep] to separate columns and
   [terminator] to terminate each row.  It fails if [header] is
   empty. *)
val make
  :  ?sep        : string
  -> ?terminator : string
  -> header      : row
  -> unit
  -> t Or_error.t

(** [with_row row t] adds [row] to the tabulator [t], returning the
   resulting tabulator.  It fails if [row] is a different length from
   [t]'s header row. *)
val with_row : row -> t -> t Or_error.t

(** [with_rule rule_char t] adds a dividing rule made up of [rule_char]
    to tabulator [t], returning the resulting tabulator. *)
val with_rule : char -> t -> t Or_error.t

(** Pretty-printing a [t] tabulates it onto the given formatter. *)
include Pretty_printer.S with type t := t
