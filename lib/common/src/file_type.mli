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

(** Heuristics for determining the vague file type of an act input file.

    These types and functions generally see use in act commands that do
    slightly different things based on whether one runs them on C, or on
    assembly, or on a Litmus test, etc. As such, they don't robustly and
    precisely identify file types. *)

(** Enumeration of the high-level file types understood by most of the bits
    of act that touch the filesystem, including the possibility of deferring
    working out the file type to a different channel. *)
type t = Assembly | C | C_litmus | Infer [@@deriving sexp]

(** {2 Classifiers with inference from filenames} *)

val is_c : Plumbing.Input.t -> t -> bool
(** [is_c infile filetype] decides whether [infile] is a C file---from its
    extension if [filetype] is [`Infer], or by whether or not [filetype] is
    [`C]. *)

val is_c_litmus : Plumbing.Input.t -> t -> bool
(** [is_c_litmus infile filetype] decides whether [infile] is a C/litmus
    file---from its extension if [filetype] is [`Infer], or by whether or
    not [filetype] is [`C_litmus]. *)

(** {2 Conversions} *)

val delitmusified : t -> t
(** [delitmusified filetype] is the type of the output of a delitmusifier if
    [filetype] is the type of the input: [`C] if [filetype] is [`C_litmus],
    and [filetype] otherwise.

    If [filetype] is [`Infer], [delitmusified filetype] is [`Infer]:
    anything using [delitmusified filetype] must take care in this case to
    make sure the correct behaviour occurs. *)
