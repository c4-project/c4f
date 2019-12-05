(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** A description of the files on which a runner task depends.

    Copy specs let remote {{!Runner} runners} copy files to and from the
    remote host, as well as validate whether the expected files actually
    exist. *)

open Base

(** {1 Specs themselves} *)

type 'path t = Directory of 'path | Files of 'path list | Nothing
[@@deriving sexp]

(** {2 Constructors} *)

val directory : 'path -> 'path t
(** [directory path] generates a directory spec over [path]. *)

val file : 'path -> 'path t
(** [file path] generates a files spec over just [path]. *)

val files : 'path list -> 'path t
(** [files paths] generates a files spec over [paths]. *)

val nothing : 'path t
(** [nothing] is the empty manifest. *)

(** {2 Using a copy-spec} *)

val validate_local : Fpath.t t -> unit Or_error.t
(** [validate_local cs] validates a local copy spec by seeing if the listed
    files and directories actually exist. *)

val get_file : string t -> string Or_error.t
(** [get_file cs] tries to extract a single file from [cs], and fails if more
    than one file, or no files, are mentioned in it. *)

val paths : 'a t -> 'a list
(** [paths cs] gets the raw list of paths covered by [cs], without further
    context. *)

(** {2 Mapping over a copy-spec} *)

val map_with_kind : 'a t -> f:([`Directory | `File] -> 'a -> 'b) -> 'b t
(** [map_with_kind cs ~f] maps [f] over all of the paths in [cs], providing
    [f] with a hint as to whether the path is supposed to be a directory or a
    file. *)

val map : 'a t -> f:('a -> 'b) -> 'b t
(** [map cs ~f] maps [f] over all of the paths in [cs]. *)

(** {1 Pairs of input and output copy specs} *)
module Pair : sig
  type 'a spec := 'a t

  type nonrec 'path t = {input: 'path t; output: 'path t} [@@deriving sexp]

  val map_specs : 'a t -> f:('a spec -> 'b spec) -> 'b t
  (** [map_specs cs_pair ~f] maps [f] over both of the specs in [cs_pair]. *)

  val map : 'a t -> f:('a -> 'b) -> 'b t
  (** [map cs_pair ~f] maps [f] over all of the paths in [cs_pair]. *)
end
