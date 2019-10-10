(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Miscellaneous string utility functions. *)

val format_for_readme : string -> string
(** [format_for_readme str] applies word-wrapping and squeezing to [str] to
    make it suitable for command READMEs. *)
