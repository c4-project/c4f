(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Type of compiler run modes. *)
type t =
  | Assembly  (** Compiling a single file to assembly. *)
  | Object  (** Compiling a single file to object code. *)
[@@deriving sexp]

include Act_utils.Enum_types.Extension_table with type t := t
