(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

val filter_files : ?ext:string -> Fpath.t list -> Fpath.t list
(** [filter_files ?ext flist] is [flist] if [ext] is absent, or the result
    of restricting [flist] to files syntactically having the extension [ext]
    otherwise. *)

val subpaths : Fpath.t -> Fpath.t list
(** [subpaths path] gets all of the syntactic subpaths of [path], according
    to [Fpath]. *)

module Unix : Fs_types.S
(** [Unix] implements {{!S} S} using Core's Unix support. *)

(* soon (** [Mock] mocks {{!S}S}, containing a mutable dummy filesystem that
   can be set up by tests. *) module Mock : sig include S end *)
