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

open Base

(** [Common] describes operations common to both input sources and output
    sinks. *)
module type Common = sig
  type t

  val of_fpath : Fpath.t -> t
  (** [of_fpath n] returns a source or sink over the file path [n]. *)

  val of_fpath_opt : Fpath.t option -> t
  (** [of_fpath_opt n] returns [of_fpath n'] if [n] is [Some n'], and the
      standard stream otherwise. *)

  val of_string_opt : string option -> t Or_error.t
  (** [of_string_opt n] tries to parse [n] as a file path if it is [Some n],
      and returns the standard stream otherwise. *)

  val to_file : t -> Fpath.t option
  (** [to_file t] returns [Some f] if [t] is a file with path [f], and
      [None] otherwise. *)

  val to_file_err : t -> Fpath.t Or_error.t
  (** [to_file_err t] behaves like {{!to_file} to_file}, but raises a mildly
      descriptive error on failure. *)

  val to_string : t -> string
  (** [to_string t] returns a descriptive string representation of [t]. *)

  include Pretty_printer.S with type t := t
end