(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** This abstract data type epresents a single projection from a local file
    to a remote file. Combined with copy specs, they track the mapping of a
    working set between machines. *)

type t [@@deriving sexp]
(** Type of projections. *)

(** {1 Constructors} *)

val make : local:Fpath.t -> remote:string -> t
(** [make ~local ~remote] makes a projection from [local] to [remote]. *)

(** {1 Accessors} *)

val local : t -> Fpath.t
(** [local proj] gets the original path of [proj]. *)

val remote : t -> string
(** [local proj] gets the projected path of [proj]. *)

(** {1 Using projections inside copy specs} *)

val project :
     Fpath.t Copy_spec.t
  -> f:([`Directory | `File] -> Fpath.t -> string)
  -> t Copy_spec.t
(** [project spec ~f] projects every copy-spec in [spec] using the projection
    function [f]. *)

val project_back :
     string Copy_spec.t
  -> f:([`Directory | `File] -> string -> Fpath.t)
  -> t Copy_spec.t
(** [project_back spec ~f] projects every copy-spec in [spec] backwards using
    the projection function [f]. *)

val all_local : t Copy_spec.t -> Fpath.t Copy_spec.t
(** [all_local spec] resolves every path in the copy spec [spec] to its local
    form. *)

val all_remote : t Copy_spec.t -> string Copy_spec.t
(** [all_remote spec] resolves every path in the copy spec [spec] to its
    remote projection. *)

val try_find : t Copy_spec.t -> Fpath.t -> string option
(** [try_find haystack needle] tries to find the remote projection of local
    path [needle] in [haystack]. *)

(** {2 Inside copy spec pairs} *)

val all_remote_pair : t Copy_spec.Pair.t -> string Copy_spec.Pair.t
(** [all_remote pair] resolves every path in the copy spec pair [pair] to its
    remote projection. *)
