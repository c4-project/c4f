(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2020 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

open Base

type t = {emits: Act_common.Id.t; version: string; name: string}
[@@deriving compare, equal, sexp]
(** Record containing information about the result of a compiler probe. *)

include Comparable.S with type t := t
(** We can compare records; the main use for this is to keep a set of 'known'
    compilers to avoid duplication. *)

include Pretty_printer.S with type t := t
(** We can also pretty-print info records. *)
