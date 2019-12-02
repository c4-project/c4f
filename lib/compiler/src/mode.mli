(* The Automagic Compiler Tormentor

   Copyright (c) 2018--2019 Matt Windsor and contributors

   ACT itself is licensed under the MIT License. See the LICENSE file in the
   project root for more information.

   ACT is based in part on code from the Herdtools7 project
   (https://github.com/herd/herdtools7) : see the LICENSE.herd file in the
   project root for more information. *)

(** Compiler run modes, and functionality for querying them. *)

(** {1 The enumeration} *)

(** Type of compiler run modes. *)
type t =
  | Assembly  (** Compiling a single file to assembly. *)
  | Object  (** Compiling a single file to object code. *)
  | Binary  (** Compiling and linking multiple files to a binary. *)
[@@deriving sexp]

include Act_utils.Enum_types.Extension_table with type t := t

(** {1 Queries} *)

val supports_multiple_inputs : t -> bool
(** [supports_multiple_inputs mode] gets whether [mode] supports multiple
    file input. Generally, only modes that involve the compiler doing its own
    linking will do so. *)
