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

(** Enumeration of the various x86 dialects, and related purposes. *)

open Utils
open Core_kernel

(** Enumeration of the various dialects of x86 syntax. *)
type t =
  | Att (* AT&T syntax (like GNU as) *)
  | Intel (* Intel syntax *)
  | Herd7 (* Herd7 syntax (somewhere in between) *)
[@@deriving sexp, compare, hash]

(** [Name_table] associates each dialect with its string name. *)
module Name_table : String_table.S with type t := t

include Identifiable.S_plain with type t := t

(** Dialect tags can be pretty-printed. *)
include Pretty_printer.S with type t := t
