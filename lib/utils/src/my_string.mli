(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Miscellaneous string utility functions. *)

(** {1 String prefixes and suffixes} *)

(** {2 Prefixes} *)

val has_prefix : string -> prefix:string -> bool
(** [has_prefix str ~prefix] is true if, and only if, [str] begins with
    [prefix]. *)

val ensure_prefix : string -> prefix:string -> string
(** [ensure_prefix str ~prefix] is [str] if [str] begins with [prefix], and
    [prefix ^ str] otherwise. *)

(** {2 Suffixes} *)

val has_suffix : string -> suffix:string -> bool
(** [has_suffix str ~suffix] is true if, and only if, [str] ends with
    [suffix]. *)

val ensure_suffix : string -> suffix:string -> string
(** [ensure_suffix str ~suffix] is [str] if [str] ends with [suffix], and
    [str ^ suffix] otherwise. *)

(** {1 Other} *)

val format_for_readme : string -> string
(** [format_for_readme str] applies word-wrapping and squeezing to [str] to
    make it suitable for command READMEs. *)
