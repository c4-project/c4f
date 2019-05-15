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

(** [My_format] contains miscellaneous formatting utilities. *)

open Base

val null_formatter : unit -> Formatter.t
(** [null_formatter ()] is a formatter that silently discards anything
    printed to it. *)

val pp_listlist :
  pp:(Formatter.t -> 'a -> unit) -> Formatter.t -> 'a list list -> unit
(** [pp_listlist ~pp o] pretty-prints a list of lists, using newlines to
    separate the first dimension and commas the second. *)

val pp_c_braces : (Formatter.t -> 'v -> unit) -> Formatter.t -> 'v -> unit
(** [pp_c_braces pi f i] wraps a vertical pretty-printer [pi] inside a
    C-style brace pair. *)

val pp_kv :
  Formatter.t -> string -> (Formatter.t -> 'v -> unit) -> 'v -> unit
(** [pp_kv f k pv v] prints a key-value pair, whose key is the string [k]
    and value is the value [v] printable by [pv], onto formatter [f]. *)
