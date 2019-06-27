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

include module type of Fs_intf
(** We keep module signatures in {{!Fs_intf} Fs_intf}. *)

val filter_files : ?ext:string -> Fpath.t list -> Fpath.t list
(** [filter_files ?ext flist is [flist] if [ext] is absent, or the result of
    restricting [flist] to files syntactically having the extension [ext]
    otherwise. *)

val subpaths : Fpath.t -> Fpath.t list
(** [subpaths path] gets all of the syntactic subpaths of [path], according
    to [Fpath]. *)

module Unix : S
(** [Unix] implements {{!S} S} using Core's Unix support. *)

(* soon (** [Mock] mocks {{!S}S}, containing a mutable dummy filesystem that
   can be set up by tests. *) module Mock : sig include S end *)
