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

(** A description of the files on which a runner task depends.

    Copy specs let remote {{!Runner}runners} copy files to and from
    the remote host, as well as validate whether the expected files
    actually exist. *)

open Base

type 'path t =
  | Directory of 'path
  | Files of 'path list
  | Nothing

(** [directory path] generates a directory spec over [path]. *)
val directory : 'path -> 'path t

(** [file path] generates a files spec over just [path]. *)
val file : 'path -> 'path t

(** [files paths] generates a files spec over [paths]. *)
val files : 'path list -> 'path t

(** [nothing] is the empty manifest. *)
val nothing : 'path t

(** [validate_local cs] validates a local copy spec by seeing
    if the listed files and directories actually exist. *)
val validate_local : Fpath.t t -> unit Or_error.t

(** [map cs ~f] maps [f] over all of the paths in [cs]. *)
val map : 'a t -> f:('a -> 'b) -> 'b t

(** Pairs of input and output copy specs. *)
module Pair : sig
  type nonrec 'path t =
    { input : 'path t
    ; output : 'path t
    }

  (** [map cs_pair ~f] maps [f] over all of the paths in [cs_pair]. *)
  val map : 'a t -> f:('a -> 'b) -> 'b t
end
