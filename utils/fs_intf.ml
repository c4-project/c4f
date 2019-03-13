(* This file is part of 'act'.

   Copyright (c) 2018, 2019 by Matt Windsor

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

(** Signatures for file-system modules. *)

open Base

(** Signature that file-system modules should implement.

    This signature exists to help mock up file-system operations in
    tests: we can pass in a mock implementation of [S] that doesn't
    actually consult the file system. *)
module type S = sig
  val get_files
    :  ?compare:(Fpath.t -> Fpath.t -> int)
    -> ?ext:string
    -> Fpath.t
    -> Fpath.t list Or_error.t
    (** [get_files ?compare ?ext path] wraps [Sys.readfiles] with
       error handling, optional extension filtering, and path sorting
       using [compare] (which, by default, is ascending collation). *)

  val check_is_dir
    :  ?on_absent:(Fpath.t -> unit Or_error.t)
    -> Fpath.t
    -> unit Or_error.t
  (** [check_is_dir ?on_absent path] checks to see if [path] is a
     directory.  If so, it returns [()].  If not, it returns an error
     if [path] is a file or indeterminate, and applies [on_absent] if
     it doesn't exist. *)

  val check_is_file
    :  ?on_absent:(Fpath.t -> unit Or_error.t)
    -> Fpath.t
    -> unit Or_error.t
    (** [check_is_file ?on_absent path] checks to see if [path] is a
       file.  If so, it returns [()].  If not, it returns an error if
       [path] is a file or indeterminate, and applies [on_absent] if
       it doesn't exist. *)

  val mkdir : Fpath.t -> unit Or_error.t
  (** [mkdir path] tries to make a directory at path [path].  If
     [path] exists and is a directory, it does nothing.  If [path]
     exists but is a file, or another error occurred, it returns an
     error message. *)

  val mkdir_p : Fpath.t -> unit Or_error.t
  (** [mkdir_p path] applies [mkdir], in turn, to each path prefix of
     [path]. *)
end
