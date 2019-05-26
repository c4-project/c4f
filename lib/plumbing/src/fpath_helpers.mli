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

(** Miscellaneous helpers for dealing with [Fpath]s. *)

open Base

val of_string : string -> Fpath.t Or_error.t
(** [of_string str] is [Fpath.of_string str], but with the error changed to
    an [Or_error.t]. *)

val of_string_option : string option -> Fpath.t option Or_error.t
(** [of_string_option str_opt] lifts [fpath_of_string] over optional
    strings. *)

val lift_str :
  string option -> f:(Fpath.t -> 'a) -> default:'a -> 'a Or_error.t
(** [lift_str str_opt ~f ~default] is a more general form of
    [of_string_option] that applies [f] over [of_string] of [str_opt] if it
    exists, and returns [default] if it doesn't. *)

val filename_no_ext : Fpath.t -> string
(** [filename_no_ext path] is the filename of [path], less any extension(s). *)
