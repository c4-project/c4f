(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Signatures for file-system modules. *)

open Base

(** Signature that file-system modules should implement.

    This signature exists to help mock up file-system operations in tests: we
    can pass in a mock implementation of [S] that doesn't actually consult
    the file system. *)
module type S = sig
  val get_files :
       ?compare:(Fpath.t -> Fpath.t -> int)
    -> ?predicate:(Fpath.t -> bool)
    -> Fpath.t
    -> Fpath.t list Or_error.t
  (** [get_files ?compare ?filtering path] wraps [Sys.readfiles] with error
      handling, filtering, and path sorting using [compare] (which, by
      default, is ascending collation). *)

  val check_is_dir :
    ?on_absent:(Fpath.t -> unit Or_error.t) -> Fpath.t -> unit Or_error.t
  (** [check_is_dir ?on_absent path] checks to see if [path] is a directory.
      If so, it returns [()]. If not, it returns an error if [path] is a file
      or indeterminate, and applies [on_absent] if it doesn't exist. *)

  val check_is_file :
    ?on_absent:(Fpath.t -> unit Or_error.t) -> Fpath.t -> unit Or_error.t
  (** [check_is_file ?on_absent path] checks to see if [path] is a file. If
      so, it returns [()]. If not, it returns an error if [path] is a file or
      indeterminate, and applies [on_absent] if it doesn't exist. *)

  val mkdir : Fpath.t -> unit Or_error.t
  (** [mkdir path] tries to make a directory at path [path]. If [path] exists
      and is a directory, it does nothing. If [path] exists but is a file, or
      another error occurred, it returns an error message. *)

  val mkdir_p : Fpath.t -> unit Or_error.t
  (** [mkdir_p path] applies [mkdir], in turn, to each path prefix of [path]. *)
end
